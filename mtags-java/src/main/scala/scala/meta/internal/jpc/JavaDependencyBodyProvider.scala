package scala.meta.internal.jpc

import java.net.URI
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.security.MessageDigest
import java.util.jar.JarFile
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.TypeKind
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.ElementKind
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.Modifier
import javax.lang.model.element.NestingKind
import javax.lang.model.element.TypeElement

import scala.collection.mutable
import scala.jdk.CollectionConverters._

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import com.sun.source.tree.MemberSelectTree
import com.sun.source.tree.MethodInvocationTree
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.Trees
import com.sun.tools.javac.api.JavacTaskImpl
import com.sun.tools.javac.code.ClassFinder
import com.sun.tools.javac.code.Symbol.ClassSymbol
import com.sun.tools.javac.code.Symbol.CompletionFailure
import com.sun.tools.javac.code.Symbol.ModuleSymbol
import com.sun.tools.javac.code.Symtab
import com.sun.tools.javac.util.Names

/**
 * Capability prototype, not an LSP admission profile. A jar entry is only a
 * lookup candidate: javac supplies every owner, subtype and override relation.
 * The request-owned inventory is never a dependency API signature model.
 */
private[jpc] final class JavaDependencyBodyProvider(
    compiler: JavaMetalsCompiler,
    compile: JavaSourceCompile,
    poll: () => Unit,
    maxClasses: Int,
    requested: Map[String, Set[(Long, Long)]]
) {
  private case class Artifact(path: Path, sha256: String)
  private val omissions = mutable.TreeSet.empty[String]
  private val artifacts = mutable.ArrayBuffer.empty[Artifact]
  private val origins = mutable.LinkedHashMap.empty[String, Vector[Artifact]]
  private val elements = compile.task.getElements
  private val types = compile.task.getTypes
  private var entriesSeen = 0
  private var bytesSeen = 0L
  private var checks = 0
  private val MaxReceiverChecks = 16 * 1024 * 1024
  private val MaxMembers = 1024 * 1024
  private val MaxProofChecks = 1024 * 1024
  private val MaxBodies = 16
  private val MaxClasspathEntries = 4096

  private def digest(path: Path): String = {
    val md = MessageDigest.getInstance("SHA-256")
    val input = Files.newInputStream(path)
    try {
      val buffer = new Array[Byte](8192)
      var count = input.read(buffer)
      var total = 0L
      while (count != -1) {
        poll()
        total += count
        if (total > MaxBytes) throw new java.io.IOException("artifact bound")
        md.update(buffer, 0, count)
        count = input.read(buffer)
      }
    } finally input.close()
    "sha256:" + md.digest().map(b => f"${b & 0xff}%02x").mkString
  }

  private val MaxBytes = 1024L * 1024 * 1024
  private val paths =
    compiler.classpath.map(_.toAbsolutePath.normalize()).distinct
  if (paths.size > MaxClasspathEntries) omissions += "CLASSPATH_ENTRY_BUDGET"
  for (path <- paths.take(MaxClasspathEntries)) {
    poll()
    if (!Files.isRegularFile(path)) omissions += "NON_JAR_CLASSPATH"
    else if (
      entriesSeen >= maxClasses || bytesSeen + Files.size(path) > MaxBytes
    ) omissions += "INVENTORY_BUDGET"
    else {
      bytesSeen += Files.size(path)
      try {
        val artifact = Artifact(path, digest(path))
        val jar = new JarFile(path.toFile)
        try {
          artifacts += artifact
          // Enumeration is deliberately bounded; do not materialize a jar's
          // complete entry list before enforcing the input limit.
          val entries = jar.entries()
          while (entries.hasMoreElements && entriesSeen < maxClasses) {
            poll()
            val entry = entries.nextElement()
            entriesSeen += 1
            // All versions supply lookup candidates only. The compiler's
            // frozen target/file manager selects the effective class, never
            // the provider runtime JDK or this inventory. Deduplicate versions
            // within one artifact, but not conflicting classpath artifacts.
            val rawName = entry.getName
            val name =
              if (
                jar.isMultiRelease && rawName.startsWith("META-INF/versions/")
              ) {
                val versioned = rawName.stripPrefix("META-INF/versions/")
                val separator = versioned.indexOf('/')
                if (
                  separator > 0 &&
                  versioned.substring(0, separator).toIntOption.exists(_ >= 9)
                )
                  versioned.substring(separator + 1)
                else rawName
              } else rawName
            if (name.endsWith(".class") && !name.startsWith("META-INF/")) {
              val binaryName = name.stripSuffix(".class").replace('/', '.')
              if (
                binaryName != "module-info" && !binaryName.endsWith(
                  "package-info"
                )
              ) {
                origins.update(
                  binaryName,
                  (origins
                    .getOrElse(binaryName, Vector.empty) :+ artifact).distinct
                )
              }
            }
          }
          if (entries.hasMoreElements) omissions += "INVENTORY_BUDGET"
        } finally jar.close()
      } catch {
        case _: java.io.IOException => omissions += "ARTIFACT_UNAVAILABLE"
      }
    }
  }

  private def artifactFor(owner: TypeElement): Option[Artifact] =
    owner match {
      case symbol: ClassSymbol if symbol.classfile != null =>
        val binary = elements.getBinaryName(owner).toString
        val candidates = origins.getOrElse(binary, Vector.empty)
        if (candidates.size != 1) None
        else {
          val uri = symbol.classfile.toUri
          if (uri.getScheme != "jar") None
          else {
            val separator = uri.getRawSchemeSpecificPart.indexOf("!/")
            if (separator < 0) None
            else {
              val origin = Paths
                .get(
                  URI.create(
                    uri.getRawSchemeSpecificPart.substring(0, separator)
                  )
                )
                .toAbsolutePath
                .normalize()
              candidates.headOption.filter(_.path == origin)
            }
          }
        }
      case _ => None
    }

  private def closed(tpe: TypeMirror, depth: Int = 0): Boolean =
    depth <= 12 && (tpe match {
      case declared: DeclaredType =>
        declared.asElement() match {
          case owner: TypeElement =>
            owner.getTypeParameters.size() == declared.getTypeArguments
              .size() &&
            declared.getTypeArguments.asScala.forall(closed(_, depth + 1)) &&
            (declared.getEnclosingType.getKind == TypeKind.NONE ||
              closed(declared.getEnclosingType, depth + 1))
          case _ => false
        }
      case _ => false
    })

  // Binary names are not canonical source names: '$' may be a literal name
  // character. Let javac load its exact class symbol from the active classpath
  // view, including MR selection, instead of resolving an invented member path.
  private val lookupBinary: String => Option[TypeElement] = compile.task match {
    case task: JavacTaskImpl =>
      val context = task.getContext
      val finder = ClassFinder.instance(context)
      val names = Names.instance(context)
      val module = Option(elements.getModuleElement(""))
        .collect { case value: ModuleSymbol =>
          value
        }
        .orElse {
          if (elements.getAllModuleElements.isEmpty)
            Some(Symtab.instance(context).noModule)
          else None
        }
      binary =>
        module.flatMap { scope =>
          try Some(finder.loadClass(scope, names.fromString(binary)))
          catch { case _: CompletionFailure => None }
        }
    case _ =>
      omissions += "BINARY_LOOKUP_UNAVAILABLE"
      _ => None
  }

  // Load/catalog work happens once per compiler request, not once per callsite.
  // Preserve frozen classpath/jar-entry order. A larger inventory must not
  // move newly discovered alphabetically earlier classes ahead of the original
  // prefix and starve its receiver proofs.
  private val candidateTypes = origins.keys.toVector.flatMap { binary =>
    poll()
    if (origins(binary).size != 1) {
      omissions += "DUPLICATE_BINARY_NAME"
      None
    } else {
      lookupBinary(binary) match {
        case Some(owner)
            if elements.getBinaryName(owner).toString == binary &&
              artifactFor(owner).isDefined &&
              (owner.getNestingKind == NestingKind.TOP_LEVEL ||
                owner.getNestingKind == NestingKind.MEMBER) =>
          if (
            owner.getKind == ElementKind.CLASS &&
            !owner.getModifiers.contains(Modifier.ABSTRACT) &&
            closed(owner.asType())
          ) Some(owner)
          else {
            if (!owner.getTypeParameters.isEmpty)
              omissions += "GENERIC_CANDIDATE_UNSUPPORTED"
            None
          }
        case _ =>
          omissions += "CLASS_SYMBOL_UNAVAILABLE"
          None
      }
    }
  }

  // Only compiler-compatible witnesses need member catalogs. Discover the
  // exact requested receiver types once, before per-callsite lookup; unrelated
  // class members must not exhaust the retained-member budget.
  private val requestedReceivers = {
    val found = mutable.ArrayBuffer.empty[TypeMirror]
    val trees = Trees.instance(compile.task)
    val positions = trees.getSourcePositions
    for (unit <- compile.allCompilationUnits) {
      val targets = requested.getOrElse(
        compiler.guessOriginalUri(unit.getSourceFile),
        Set.empty
      )
      if (targets.nonEmpty) {
        new TreePathScanner[Unit, Unit] {
          override def visitMethodInvocation(
              call: MethodInvocationTree,
              unused: Unit
          ): Unit = {
            poll()
            if (
              targets.contains(
                (
                  positions.getStartPosition(unit, call),
                  positions.getEndPosition(unit, call)
                )
              )
            ) {
              call.getMethodSelect match {
                case selected: MemberSelectTree =>
                  val selectedPath = new TreePath(getCurrentPath, selected)
                  trees.getElement(selectedPath) match {
                    case method: ExecutableElement
                        if method.getModifiers.contains(Modifier.ABSTRACT) &&
                          !method.getModifiers.contains(Modifier.STATIC) &&
                          method.getTypeParameters.isEmpty =>
                      val receiver = trees.getTypeMirror(
                        new TreePath(selectedPath, selected.getExpression)
                      )
                      if (closed(receiver)) found += receiver
                    case _ => ()
                  }
                case _ => ()
              }
            }
            super.visitMethodInvocation(call, unused)
          }
        }.scan(unit, ())
      }
    }
    found.toVector
  }
  private var receiverChecks = 0
  private val applicableCandidateTypes = candidateTypes.filter { witness =>
    requestedReceivers.exists { receiver =>
      poll()
      if (receiverChecks >= MaxReceiverChecks) {
        omissions += "RECEIVER_INVENTORY_BUDGET"
        false
      } else {
        receiverChecks += 1
        types.isSubtype(witness.asType(), receiver)
      }
    }
  }
  private var indexedMembers = 0
  private var membersSeen = 0L
  private val methodsByName = applicableCandidateTypes
    .flatMap { witness =>
      poll()
      val members = elements.getAllMembers(witness).asScala
      membersSeen += members.size
      val remaining = math.max(0, MaxMembers - indexedMembers)
      if (members.size > remaining) omissions += "MEMBER_INVENTORY_BUDGET"
      indexedMembers += math.min(members.size, remaining)
      members.take(remaining).collect {
        case method: ExecutableElement
            if method.getKind == ElementKind.METHOD &&
              !method.getModifiers.contains(Modifier.ABSTRACT) &&
              !method.getModifiers.contains(Modifier.STATIC) &&
              !method.getModifiers.contains(Modifier.PRIVATE) &&
              !method.getModifiers.contains(Modifier.NATIVE) =>
          (method.getSimpleName.toString, (witness, method))
      }
    }
    .groupMap(_._1)(_._2)

  def discover(path: TreePath, receiver: TypeMirror): JsonObject = {
    poll()
    val out = new JsonObject()
    val bodies = new JsonArray()
    out.add("bodies", bodies)
    out.addProperty("populationClosed", false)
    var status = "SCANNED"
    Trees.instance(compile.task).getElement(path) match {
      case selected: ExecutableElement
          if selected.getModifiers.contains(Modifier.ABSTRACT) &&
            !selected.getModifiers.contains(Modifier.STATIC) &&
            selected.getTypeParameters.isEmpty && closed(receiver) =>
        selected.getEnclosingElement match {
          case selectedOwner: TypeElement
              if artifactFor(selectedOwner).isDefined =>
            val selectedArtifact = artifactFor(selectedOwner).get
            out.addProperty(
              "selectedArtifactUri",
              selectedArtifact.path.toUri.toString
            )
            out.addProperty("selectedArtifactSha256", selectedArtifact.sha256)
            val seen = mutable.Set.empty[String]
            for (
              (witness, method) <- methodsByName.getOrElse(
                selected.getSimpleName.toString,
                Vector.empty
              )
            ) {
              poll()
              if (checks >= MaxProofChecks || bodies.size() >= MaxBodies) {
                omissions += "PROOF_BUDGET"
              } else {
                checks += 1
                if (
                  types.isSubtype(witness.asType(), receiver) &&
                  elements.overrides(method, selected, witness) &&
                  method.getTypeParameters.isEmpty
                ) {
                  method.getEnclosingElement match {
                    case owner: TypeElement =>
                      for {
                        artifact <- artifactFor(owner)
                        witnessArtifact <- artifactFor(witness)
                        callable <- JavaReceiverSelection.callable(
                          compile.task,
                          method
                        )
                      } {
                        val key = callable.toString
                        if (seen.add(key)) {
                          callable.addProperty(
                            "artifactUri",
                            artifact.path.toUri.toString
                          )
                          callable.addProperty(
                            "artifactSha256",
                            artifact.sha256
                          )
                          callable.addProperty(
                            "receiverWitness",
                            witness.getQualifiedName.toString
                          )
                          callable.addProperty(
                            "witnessArtifactUri",
                            witnessArtifact.path.toUri.toString
                          )
                          callable.addProperty(
                            "witnessArtifactSha256",
                            witnessArtifact.sha256
                          )
                          callable.addProperty(
                            "proof",
                            "JAVAC_SUBTYPE_AND_OVERRIDE"
                          )
                          bodies.add(callable)
                        }
                      }
                    case _ => omissions += "BODY_OWNER_UNAVAILABLE"
                  }
                }
              }
            }
          case _ => status = "SELECTED_ARTIFACT_UNAVAILABLE"
        }
      case _ => status = "UNSUPPORTED_SELECTION_OR_RECEIVER"
    }
    out.addProperty("status", status)
    val reasons = new JsonArray()
    omissions.foreach(reasons.add)
    out.add("omissions", reasons)
    out.addProperty("inventoryEntriesSeen", entriesSeen)
    out.addProperty("inventoryClassCount", origins.size)
    out.addProperty("candidateTypeCount", candidateTypes.size)
    // Request-cumulative counters recur on each callsite row: consumers must
    // take maxima, not sum rows. None of these counters proves closure.
    val work = new JsonObject()
    work.addProperty("inventoryEntryLimit", maxClasses)
    work.addProperty("artifactByteLimit", MaxBytes)
    work.addProperty("artifactBytesSeen", bytesSeen)
    work.addProperty("classpathEntryLimit", MaxClasspathEntries)
    work.addProperty("classpathEntriesPlanned", paths.size)
    work.addProperty(
      "classpathEntriesAttempted",
      math.min(paths.size, MaxClasspathEntries)
    )
    work.addProperty("requestedReceiverCount", requestedReceivers.size)
    work.addProperty("receiverCheckLimit", MaxReceiverChecks)
    work.addProperty("receiverChecks", receiverChecks)
    work.addProperty("applicableCandidateCount", applicableCandidateTypes.size)
    work.addProperty("memberLimit", MaxMembers)
    work.addProperty("membersSeen", membersSeen)
    work.addProperty("membersIndexed", indexedMembers)
    work.addProperty("proofCheckLimit", MaxProofChecks)
    work.addProperty("proofChecks", checks)
    work.addProperty("bodyLimit", MaxBodies)
    work.addProperty("bodyCount", bodies.size())
    out.add("workMetrics", work)
    out
  }

  def unchanged(): Boolean = artifacts.forall { artifact =>
    poll()
    try digest(artifact.path) == artifact.sha256
    catch { case _: java.io.IOException => false }
  }
}
