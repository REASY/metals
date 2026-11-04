package scala.meta.internal.pc

import java.nio.charset.StandardCharsets
import java.util.Collections

import scala.collection.mutable
import scala.jdk.CollectionConverters._

import scala.meta.internal.mtags.MD5
import scala.meta.pc.ScalaConstructorEvidence
import scala.meta.pc.ScalaConstructorEvidence.Invocation
import scala.meta.pc.ScalaConstructorEvidence.Parameter
import scala.meta.pc.ScalaConstructorEvidence.TypeReference
import scala.meta.pc.VirtualFileParams

import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.{Position => LspPosition}

/** Exact typed constructor applications. Unsupported forms never expose partial signatures. */
final class ScalaConstructorEvidenceProvider(
    val compiler: MetalsGlobal,
    params: VirtualFileParams
) {
  import compiler._

  private val code = params.text()
  private val md5 = MD5.compute(code)
  private val uri = params.uri().toString
  private lazy val lineStarts = (Vector(0) ++ code.indices.collect {
    case i if code.charAt(i) == '\n' => i + 1
  }).toArray

  private def unavailable(reason: String): ScalaConstructorEvidence =
    ScalaConstructorEvidence.unavailable(uri, md5, reason)

  private def range(pos: Position): Option[Range] =
    if (
      !pos.isRange || pos.start < 0 || pos.end > code.length || pos.start >= pos.end
    )
      None
    else {
      def at(offset: Int): LspPosition = {
        val found = java.util.Arrays.binarySearch(lineStarts, offset)
        val line = if (found >= 0) found else -found - 2
        new LspPosition(line, offset - lineStarts(line))
      }
      Some(new Range(at(pos.start), at(pos.end)))
    }

  private final class TypeBudget {
    var nodes = 0
    def project(tpe: Type, depth: Int = 0): Option[TypeReference] = {
      nodes += 1
      if (nodes > 256 || depth > 16) None
      else
        tpe.dealias match {
          case TypeRef(prefix, symbol, arguments)
              if symbol != NoSymbol && symbol.isClass && !symbol.isTypeParameter &&
                symbol != definitions.RepeatedParamClass &&
                symbol != definitions.JavaRepeatedParamClass &&
                symbol != definitions.ByNameParamClass &&
                arguments.size <= 128 &&
                (symbol.owner.isPackageClass || symbol.owner.isModuleClass) &&
                (prefix == NoPrefix || prefix.typeSymbol.isPackageClass || prefix.typeSymbol.isModuleClass) =>
            val projected = arguments.map(project(_, depth + 1))
            val identity = semanticdbSymbol(symbol)
            if (identity.isEmpty || projected.exists(_.isEmpty)) None
            else Some(new TypeReference(identity, projected.flatten.asJava))
          case _ => None
        }
    }
  }

  def evidence(): ScalaConstructorEvidence = {
    params.checkCanceled()
    if (code.getBytes(StandardCharsets.UTF_8).length > 4 * 1024 * 1024)
      unavailable("SOURCE_BOUND")
    else {
      val unit = addCompilationUnit(code, uri, cursor = None, forceNew = false)
      typeCheck(unit)
      params.checkCanceled()
      // BatchSourceFile adds exactly one final newline when the input lacks it.
      // Hash/correlation still use the supplied buffer, never a disk reread.
      val compilerCode = unit.source.content.mkString
      if (compilerCode != code && compilerCode != code + "\n")
        unavailable("SOURCE_CHANGED")
      else if (
        semanticdbDiagnosticsOf(unit).exists(
          _.severity == scala.meta.internal.semanticdb.Diagnostic.Severity.ERROR
        )
      )
        unavailable("COMPILER_ERROR")
      else {
        val invocations = mutable.ListBuffer.empty[Invocation]
        var visited = 0
        var boundExceeded = false
        object collect extends Traverser {
          override def traverse(tree: Tree): Unit = {
            params.checkCanceled()
            visited += 1
            if (visited > 50000 || invocations.size >= 4096)
              boundExceeded = true
            else {
              tree match {
                case apply @ Apply(select @ Select(receiver: New, _), arguments)
                    if select.symbol != NoSymbol && select.symbol.isConstructor =>
                  val selected = select.symbol
                  val invocationRange = range(apply.pos)
                  val calleeRange = range(receiver.tpt.pos)
                  val budget = new TypeBudget
                  val clauses = selected.info.paramLists
                  val projected = clauses.map(_.map { parameter =>
                    budget.project(parameter.info).map { tpe =>
                      new Parameter(
                        semanticdbSymbol(parameter),
                        parameter.name.decodedName.toString,
                        tpe
                      )
                    }
                  })
                  val constructed = budget.project(receiver.tpt.tpe)
                  val selectedSymbol = semanticdbSymbol(selected)
                  val ownerSymbol = semanticdbSymbol(selected.owner)
                  val reason =
                    if (selected.owner.isAnonymousClass) "ANONYMOUS_CONSTRUCTOR"
                    else if (
                      invocationRange.isEmpty || calleeRange.isEmpty ||
                      apply.pos.start > receiver.tpt.pos.start ||
                      apply.pos.end < receiver.tpt.pos.end
                    )
                      "SOURCE_RANGE_UNAVAILABLE"
                    else if (
                      selected.owner.typeParams.nonEmpty ||
                      clauses.size != 1 || clauses.head.size != arguments.size ||
                      clauses.head.size > 128 || clauses.head.exists(
                        _.isImplicit
                      ) ||
                      projected.flatten.exists(_.isEmpty) ||
                      constructed.isEmpty
                    )
                      "UNSUPPORTED_SIGNATURE"
                    else if (
                      selectedSymbol.isEmpty || ownerSymbol.isEmpty ||
                      projected.flatten.flatten.exists(_.symbol.isEmpty)
                    )
                      "SYMBOL_UNAVAILABLE"
                    else ""
                  invocations += new Invocation(
                    invocationRange.orNull,
                    calleeRange.orNull,
                    if (reason.isEmpty) "AVAILABLE" else "UNAVAILABLE",
                    reason,
                    if (reason.isEmpty) selectedSymbol else null,
                    if (reason.isEmpty) ownerSymbol else null,
                    if (reason.isEmpty) selected.owner.fullName else null,
                    if (reason.isEmpty) projected.map(_.flatten.asJava).asJava
                    else Collections.emptyList(),
                    if (reason.isEmpty) constructed.orNull else null
                  )
                case _ => ()
              }
              super.traverse(tree)
            }
          }
        }
        collect.traverse(unit.body)
        if (boundExceeded) unavailable("DOCUMENT_FACT_BOUND")
        else
          new ScalaConstructorEvidence(
            uri,
            md5,
            "AVAILABLE",
            "",
            invocations.toSeq.asJava
          )
      }
    }
  }
}
