package scala.meta.internal.jpc

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import javax.lang.model.`type`.ArrayType
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.TypeKind
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.TypeElement
import javax.tools.Diagnostic.Kind.ERROR

import scala.jdk.CollectionConverters._

import scala.meta.pc.VirtualFileParams

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import com.sun.source.tree.CompilationUnitTree
import com.sun.source.tree.MemberSelectTree
import com.sun.source.tree.MethodInvocationTree
import com.sun.source.tree.NewClassTree
import com.sun.source.tree.Tree
import com.sun.source.util.TreePath
import com.sun.source.util.TreePathScanner
import com.sun.source.util.Trees

/**
 * Bounded compiler receiver evidence. Legacy experimental entrypoints retain
 * their original profile; the service route also retains selected callables.
 * A compiler expression type is distinct from a selected declaration's owner or
 * generic return signature. This response makes no dispatch/closure claim.
 */
private[jpc] class JavaReceiverTypeProvider(compiler: JavaMetalsCompiler) {
  import JavaReceiverTypeProvider._

  def documents(
      params: List[VirtualFileParams],
      requested: Option[Map[String, Set[(Long, Long)]]] = None,
      retainSelection: Boolean = false,
      dependencyBodyLimit: Option[Int] = None,
      retainOrigin: Boolean = false
  ): JsonObject = {
    val ordered = params.sortBy(_.uri().toString)
    ordered.foreach(_.checkCanceled())
    if (
      ordered.size > MaxFiles ||
      ordered.map(_.uri()).distinct.size != ordered.size ||
      ordered.exists(_.text().getBytes(UTF_8).length > MaxSourceBytes) ||
      ordered.map(_.text().getBytes(UTF_8).length.toLong).sum > MaxBatchBytes
    ) unavailable(ordered, "INPUT_LIMIT_OR_DUPLICATE_URI")
    else if (ordered.isEmpty) unavailable(ordered, "EMPTY_REQUEST")
    else {
      val compile = compiler.batchCompilationTask(ordered).withAnalyzePhase()
      ordered.foreach(_.checkCanceled())
      val errors = compile.listener.diagnostics.count(_.getKind == ERROR)
      val dependencyBodies =
        if (errors == 0)
          dependencyBodyLimit.map(limit =>
            new JavaDependencyBodyProvider(
              compiler,
              compile,
              () => ordered.foreach(_.checkCanceled()),
              limit,
              requested.getOrElse(Map.empty)
            )
          )
        else None
      val documents = new JsonArray()
      for (param <- ordered) {
        val matches = compile.allCompilationUnits.filter(cu =>
          compiler.guessOriginalUri(cu.getSourceFile) == param.uri().toString
        )
        val document =
          if (errors > 0) emptyDocument(param, "COMPILER_ERRORS")
          else if (matches.size != 1)
            emptyDocument(param, "SOURCE_UNIT_UNAVAILABLE")
          else if (
            matches.head.getSourceFile.getCharContent(true).toString != param
              .text()
          ) emptyDocument(param, "SOURCE_CONTENT_MISMATCH")
          else
            collect(
              compile,
              matches.head,
              param,
              requested.map(_.getOrElse(param.uri().toString, Set.empty)),
              retainSelection,
              dependencyBodies,
              retainOrigin
            )
        document.addProperty("compilerErrorCount", errors)
        if (errors > 0)
          document.add(
            "compilerDiagnostics",
            errorDiagnostics(compile, () => ordered.foreach(_.checkCanceled()))
          )
        requested.foreach { targets =>
          completeTargets(
            document,
            targets.getOrElse(param.uri().toString, Set.empty)
          )
        }
        documents.add(document)
      }
      if (dependencyBodies.exists(provider => !provider.unchanged()))
        unavailable(ordered, "CLASSPATH_CHANGED")
      else if (
        dependencyBodies.isDefined &&
        compile.listener.diagnostics.exists(_.getKind == ERROR)
      ) unavailable(ordered, "COMPILER_ERRORS")
      else batch(documents)
    }
  }

  // Failure evidence only: retain javac's structured code and location, never
  // formatted messages (which can contain source expressions or literals).
  // This separately versioned optional catalog does not admit partial answers.
  private def errorDiagnostics(
      compile: JavaSourceCompile,
      checkCanceled: () => Unit
  ): JsonObject = {
    val diagnostics = new JsonArray()
    var total = 0
    var bytes = 0
    for (
      diagnostic <- compile.listener.diagnostics if diagnostic.getKind == ERROR
    ) {
      checkCanceled()
      total += 1
      val row = new JsonObject()
      val code = Option(diagnostic.getCode).filter(value =>
        value.length <= 256 && value.forall(c =>
          c.isLetterOrDigit || c == '.' || c == '_'
        )
      )
      row.addProperty("code", code.getOrElse("UNAVAILABLE_CODE"))
      Option(diagnostic.getSource).foreach { source =>
        val uri = compiler.guessOriginalUri(source)
        if (uri.length <= 8192 && !uri.exists(_.isControl))
          row.addProperty("sourceUri", uri)
      }
      row.addProperty("startUtf16", diagnostic.getStartPosition)
      row.addProperty("endUtf16", diagnostic.getEndPosition)
      row.addProperty("positionUtf16", diagnostic.getPosition)
      val size = row.toString.getBytes(UTF_8).length
      if (
        diagnostics.size < MaxErrorDiagnostics && bytes + size <= MaxErrorDiagnosticBytes
      ) {
        diagnostics.add(row)
        bytes += size
      }
    }
    val result = new JsonObject()
    result.addProperty("schemaVersion", "metals-java-compiler-diagnostics.v1")
    result.addProperty("totalErrors", total)
    result.addProperty("omittedErrors", total - diagnostics.size)
    result.add("errors", diagnostics)
    result
  }

  private def collect(
      compile: JavaSourceCompile,
      unit: CompilationUnitTree,
      params: VirtualFileParams,
      requested: Option[Set[(Long, Long)]],
      retainSelection: Boolean,
      dependencyBodies: Option[JavaDependencyBodyProvider],
      retainOrigin: Boolean
  ): JsonObject = {
    val trees = Trees.instance(compile.task)
    val positions = trees.getSourcePositions
    val rows = new JsonArray()
    var omitted = 0
    var retainedBytes = 0
    new TreePathScanner[Unit, Unit] {
      // Selection does not require a value receiver. Constructors and implicit
      // calls must not be made unreachable by a receiver-only planning gate.
      private def selectionOnly(call: Tree, selectedPath: TreePath): Unit = {
        params.checkCanceled()
        val start = positions.getStartPosition(unit, call)
        val end = positions.getEndPosition(unit, call)
        if (
          retainSelection && dependencyBodies.isEmpty &&
          requested.exists(_.contains((start, end))) &&
          start >= 0 && end > start && end <= params.text().length
        ) {
          if (rows.size() >= MaxSites) omitted += 1
          else {
            val row = new JsonObject()
            val span = new JsonObject()
            span.addProperty("startUtf16", start)
            span.addProperty("endUtf16", end)
            row.add("callRange", span)
            row.addProperty("callSyntax", call.getKind.name())
            JavaReceiverSelection.selected(compile.task, selectedPath) match {
              case Some(selection) =>
                row.addProperty("status", "SELECTED")
                row.add("callName", selection.get("member"))
                row.add("selectedCallable", selection)
                if (retainOrigin)
                  row.add(
                    "selectedOrigin",
                    JavaReceiverSelection.origin(compile.task, selectedPath)
                  )
              case None => row.addProperty("status", "SELECTION_UNAVAILABLE")
            }
            val bytes = row.toString.getBytes(UTF_8).length
            if (
              bytes > MaxRowBytes || retainedBytes + bytes > MaxDocumentBytes
            ) {
              omitted += 1
              row.remove("selectedCallable")
              row.remove("selectedOrigin")
              row.remove("callName")
              row.addProperty("status", "OUTPUT_BUDGET")
            } else retainedBytes += bytes
            rows.add(row)
          }
        }
      }

      override def visitNewClass(call: NewClassTree, unused: Unit): Unit = {
        selectionOnly(call, getCurrentPath)
        super.visitNewClass(call, unused)
      }

      override def visitMethodInvocation(
          call: MethodInvocationTree,
          unused: Unit
      ): Unit = {
        params.checkCanceled()
        call.getMethodSelect match {
          case selected: MemberSelectTree
              if requested.forall(
                _.contains(
                  (
                    positions.getStartPosition(unit, call),
                    positions.getEndPosition(unit, call)
                  )
                )
              ) =>
            if (rows.size() >= MaxSites) omitted += 1
            else {
              val receiverPath = new TreePath(
                new TreePath(getCurrentPath, selected),
                selected.getExpression
              )
              val receiverType = trees.getTypeMirror(receiverPath)
              val row = new JsonObject()
              row.addProperty("callName", selected.getIdentifier.toString)
              row.addProperty(
                "receiverSyntax",
                selected.getExpression.getKind.name()
              )
              row.addProperty(
                "receiverRole",
                if (trees.getElement(receiverPath).isInstanceOf[TypeElement])
                  "TYPE_QUALIFIER"
                else "VALUE"
              )
              def range(tree: Tree): Option[JsonObject] = {
                val start = positions.getStartPosition(unit, tree)
                val end = positions.getEndPosition(unit, tree)
                if (start < 0 || end < start || end > params.text().length) None
                else {
                  val value = new JsonObject()
                  value.addProperty("startUtf16", start)
                  value.addProperty("endUtf16", end)
                  Some(value)
                }
              }
              val callRange = range(call)
              val receiverRange = range(selected.getExpression)
              (callRange, receiverRange) match {
                case (Some(callSpan), Some(receiverSpan)) =>
                  row.add("callRange", callSpan)
                  row.add("receiverRange", receiverSpan)
                  typeFact(receiverType, 0, new TypeBudget) match {
                    case Some(fact) =>
                      row.addProperty("status", "TYPED")
                      row.add("receiverType", fact)
                    case None =>
                      row.addProperty("status", "UNSUPPORTED_TYPE")
                  }
                case _ =>
                  row.addProperty("status", "SOURCE_RANGE_UNAVAILABLE")
              }
              if (retainSelection) {
                JavaReceiverSelection.selected(
                  compile.task,
                  new TreePath(getCurrentPath, selected)
                ) match {
                  case Some(selection) => row.add("selectedCallable", selection)
                  case None =>
                    row.remove("receiverType")
                    row.addProperty("status", "SELECTION_UNAVAILABLE")
                }
                if (retainOrigin && row.has("selectedCallable"))
                  row.add(
                    "selectedOrigin",
                    JavaReceiverSelection.origin(
                      compile.task,
                      new TreePath(getCurrentPath, selected)
                    )
                  )
              }
              if (row.get("status").getAsString == "TYPED") {
                dependencyBodies.foreach { provider =>
                  row.add(
                    "dependencyBodies",
                    provider.discover(
                      new TreePath(getCurrentPath, selected),
                      receiverType
                    )
                  )
                }
              }
              val bytes = row.toString.getBytes(UTF_8).length
              if (
                bytes > MaxRowBytes ||
                retainedBytes + bytes > MaxDocumentBytes
              ) {
                omitted += 1
                if (requested.isDefined) {
                  val fallback = new JsonObject()
                  callRange.foreach(fallback.add("callRange", _))
                  fallback.addProperty("status", "OUTPUT_BUDGET")
                  rows.add(fallback)
                }
              } else {
                retainedBytes += bytes
                rows.add(row)
              }
            }
          case _ =>
            selectionOnly(
              call,
              new TreePath(getCurrentPath, call.getMethodSelect)
            )
        }
        super.visitMethodInvocation(call, unused)
      }
    }.scan(unit, ())
    val result =
      emptyDocument(params, if (omitted == 0) "COMPLETE" else "PARTIAL")
    result.add("receiverSites", rows)
    result.addProperty("omittedReceiverSites", omitted)
    result
  }
}

private[jpc] object JavaReceiverTypeProvider {
  val SchemaVersion = "metals-java-receiver-types.experimental.v1"
  val TargetedSchemaVersion =
    "metals-java-receiver-types.experimental.targeted.v1"
  val SemanticSchemaVersion = "metals-java-receiver-types.v2"
  val MaxFiles = 16
  val MaxSourceBytes: Int = 1024 * 1024
  val MaxBatchBytes: Int = 4 * 1024 * 1024
  val MaxSites = 256
  val MaxRowBytes: Int = 16 * 1024
  val MaxDocumentBytes: Int = 1024 * 1024
  val MaxErrorDiagnostics = 32
  val MaxErrorDiagnosticBytes: Int = 32 * 1024

  def validateTargets(
      params: List[VirtualFileParams],
      targets: List[JavaReceiverTarget]
  ): Either[String, Map[String, Set[(Long, Long)]]] = {
    params.foreach(_.checkCanceled())
    if (
      params.size > MaxFiles || params
        .map(_.uri())
        .distinct
        .size != params.size ||
      params.exists(p =>
        p.uri().toString.length > 8192 || p.text().length > MaxSourceBytes
      ) ||
      params.map(_.text().getBytes(UTF_8).length.toLong).sum > MaxBatchBytes ||
      params.exists(_.text().getBytes(UTF_8).length > MaxSourceBytes)
    )
      Left("INPUT_LIMIT_OR_DUPLICATE_URI")
    else if (targets.isEmpty || targets.size > MaxSites * MaxFiles)
      Left("TARGET_COUNT_LIMIT")
    else {
      val sources = params.map(p => p.uri() -> p).toMap
      val hashes = params.map(p => p.uri() -> sourceHash(p.text())).toMap
      val grouped = targets.groupBy(_.uri)
      def boundary(text: String, offset: Int): Boolean =
        offset == 0 || offset == text.length ||
          !(Character.isHighSurrogate(text.charAt(offset - 1)) && Character
            .isLowSurrogate(text.charAt(offset)))
      if (grouped.values.exists(_.size > MaxSites)) Left("TARGET_COUNT_LIMIT")
      else if (targets.exists(t => !sources.contains(t.uri)))
        Left("TARGET_URI_UNAVAILABLE")
      else if (targets.exists(t => hashes(t.uri) != t.sourceSha256))
        Left("TARGET_SOURCE_MISMATCH")
      else if (
        targets.exists(t =>
          t.startUtf16 < 0 || t.endUtf16 <= t.startUtf16 || t.endUtf16 > sources(
            t.uri
          ).text().length
        )
      ) Left("INVALID_TARGET_RANGE")
      else if (
        targets.exists(t =>
          !boundary(sources(t.uri).text(), t.startUtf16) || !boundary(
            sources(t.uri).text(),
            t.endUtf16
          )
        )
      ) Left("INVALID_TARGET_RANGE")
      else if (
        grouped.values.exists(ts =>
          ts.map(t => (t.startUtf16, t.endUtf16)).distinct.size != ts.size
        )
      ) Left("DUPLICATE_TARGET_RANGE")
      else
        Right(grouped.map { case (uri, ts) =>
          uri.toString -> ts
            .map(t => (t.startUtf16.toLong, t.endUtf16.toLong))
            .toSet
        })
    }
  }

  def targetRequestHash(targets: List[JavaReceiverTarget]): String = {
    val canonical = new JsonArray()
    targets.sortBy(t => (t.uri.toString, t.startUtf16, t.endUtf16)).foreach {
      t =>
        val row = new JsonArray()
        row.add(t.uri.toString)
        row.add(t.sourceSha256)
        row.add(t.startUtf16)
        row.add(t.endUtf16)
        canonical.add(row)
    }
    sourceHash(canonical.toString)
  }

  private def completeTargets(
      document: JsonObject,
      targets: Set[(Long, Long)]
  ): Unit = {
    val oldStatus = document.get("status").getAsString
    val existing = document
      .getAsJsonArray("receiverSites")
      .asScala
      .map(_.getAsJsonObject)
      .filter(_.has("callRange"))
      .groupBy { row =>
        val span = row.getAsJsonObject("callRange")
        (span.get("startUtf16").getAsLong, span.get("endUtf16").getAsLong)
      }
    def fallback(span: (Long, Long), status: String): JsonObject = {
      val row = new JsonObject()
      val range = new JsonObject()
      range.addProperty("startUtf16", span._1)
      range.addProperty("endUtf16", span._2)
      row.add("callRange", range)
      row.addProperty("status", status)
      row
    }
    val rows = new JsonArray()
    targets.toVector.sorted.foreach { span =>
      existing.get(span) match {
        case Some(found) if found.size == 1 => rows.add(found.head)
        case Some(_) => rows.add(fallback(span, "AMBIGUOUS_SOURCE_RANGE"))
        case None =>
          rows.add(
            fallback(
              span,
              if (oldStatus == "COMPLETE" || oldStatus == "PARTIAL")
                "TARGET_NOT_FOUND_OR_UNSUPPORTED_SYNTAX"
              else oldStatus
            )
          )
      }
    }
    document.add("receiverSites", rows)
    document.addProperty("requestedReceiverSites", targets.size)
    if (document.toString.getBytes(UTF_8).length > MaxDocumentBytes) {
      val unavailable = new JsonArray()
      targets.toVector.sorted.foreach(span =>
        unavailable.add(fallback(span, "OUTPUT_BUDGET"))
      )
      document.add("receiverSites", unavailable)
      document.addProperty("omittedReceiverSites", targets.size)
      document.addProperty("status", "PARTIAL")
    }
  }

  private class TypeBudget {
    var remaining = 64
  }

  // Do not silently erase type variables, captures, wildcards or intersections
  // to their bounds. Those need a richer explicitly correlated type contract.
  private def typeFact(
      tpe: TypeMirror,
      depth: Int,
      budget: TypeBudget
  ): Option[JsonObject] = {
    if (tpe == null || depth > 8 || budget.remaining == 0) None
    else {
      budget.remaining -= 1
      tpe match {
        case declared: DeclaredType
            if declared.getKind == TypeKind.DECLARED &&
              declared.getEnclosingType.getKind == TypeKind.NONE =>
          declared.asElement() match {
            case element: TypeElement =>
              val name = element.getQualifiedName.toString
              val arguments = declared.getTypeArguments.asScala
                .map(arg => typeFact(arg, depth + 1, budget))
              if (
                name.isEmpty || name.length > 1024 ||
                arguments.exists(_.isEmpty)
              ) None
              else {
                val result = new JsonObject()
                result.addProperty("kind", "DECLARED")
                result.addProperty("qualifiedName", name)
                val args = new JsonArray()
                arguments.flatten.foreach(args.add)
                result.add("typeArguments", args)
                Some(result)
              }
            case _ => None
          }
        case array: ArrayType =>
          typeFact(array.getComponentType, depth + 1, budget).map { component =>
            val result = new JsonObject()
            result.addProperty("kind", "ARRAY")
            result.add("componentType", component)
            result
          }
        case primitive if primitive.getKind.isPrimitive =>
          val result = new JsonObject()
          result.addProperty("kind", primitive.getKind.name())
          Some(result)
        case _ => None
      }
    }
  }

  def unavailable(
      params: List[VirtualFileParams],
      reason: String
  ): JsonObject = {
    val result = batch(new JsonArray())
    result.addProperty("status", reason)
    result.addProperty("requestedDocumentCount", params.size)
    result
  }

  private def emptyDocument(
      params: VirtualFileParams,
      status: String
  ): JsonObject = {
    val result = new JsonObject()
    result.addProperty("uri", params.uri().toString)
    result.addProperty("sourceSha256", sourceHash(params.text()))
    result.addProperty("status", status)
    result.add("receiverSites", new JsonArray())
    result.addProperty("omittedReceiverSites", 0)
    result
  }

  private def sourceHash(text: String): String = {
    val hash = MessageDigest
      .getInstance("SHA-256")
      .digest(text.getBytes(UTF_8))
      .map(b => f"${b & 0xff}%02x")
      .mkString
    s"sha256:$hash"
  }

  private def batch(documents: JsonArray): JsonObject = {
    val result = new JsonObject()
    result.addProperty("schemaVersion", SchemaVersion)
    result.addProperty("status", "DOCUMENT_RESULTS")
    result.add("documents", documents)
    result
  }
}
