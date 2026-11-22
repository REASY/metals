package tests.pc

import java.net.URI
import java.nio.file.Files
import java.nio.file.Paths

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.jsemanticdb.Semanticdb

import tests.BasePCSuite

class SelectedFormalSemanticdbSuite extends BasePCSuite {
  test("generated-apply-retains-qualified-formals-and-selection") {
    val source =
      """package model {
        |  object Flow { final case class RequestContext(value: String) }
        |}
        |package alternate {
        |  object Flow { final case class RequestContext(value: Int) }
        |}
        |package model.pool {
        |  import model.Flow.RequestContext
        |  import java.util.concurrent._
        |  import scala.concurrent.duration._
        |  final case class Waiting(ongoingRequest: RequestContext, done: Boolean)
        |  final case class ShadowWaiting(ongoingRequest: alternate.Flow.RequestContext, done: Boolean)
        |  final case class Overloaded(ongoingRequest: RequestContext)
        |  object Overloaded {
        |    def apply(ongoingRequest: alternate.Flow.RequestContext): Overloaded =
        |      new Overloaded(model.Flow.RequestContext(ongoingRequest.value.toString))
        |  }
        |  object Caller {
        |    def waiting(request: RequestContext): Waiting = Waiting(request, false)
        |    def shadow(request: alternate.Flow.RequestContext): ShadowWaiting = ShadowWaiting(request, false)
        |    def overloaded(request: RequestContext): Overloaded = Overloaded(request)
        |  }
        |}
        |""".stripMargin
    val bytes = presentationCompiler
      .semanticdbTextDocument(new URI("file:///Selected.scala"), source)
      .get()
    val document = Semanticdb.TextDocument.parseFrom(bytes)
    sys.env.get("CODEHOP_RAW_SEMANTICDB_OUT").foreach { directory =>
      val root = Paths.get(directory)
      Files.createDirectory(root)
      Files.write(root.resolve("document.semanticdb"), bytes)
      Files.write(
        root.resolve("document.txt"),
        document.toString.getBytes("UTF-8")
      )
    }
    assertEquals(document.getDiagnosticsCount, 0)
    val symbols =
      document.getSymbolsList.asScala.map(info => info.getSymbol -> info).toMap
    val waiting = symbols("model/pool/Waiting.apply().")
    assert(waiting.hasSignature, "generated apply method signature absent")
    val parameter = symbols("model/pool/Waiting.apply().(ongoingRequest)")
    assertEquals(
      parameter.getSignature.getValueSignature.getTpe.getTypeRef.getSymbol,
      "model/Flow.RequestContext#"
    )
    val shadow = symbols("model/pool/ShadowWaiting.apply().(ongoingRequest)")
    assertEquals(
      shadow.getSignature.getValueSignature.getTpe.getTypeRef.getSymbol,
      "alternate/Flow.RequestContext#"
    )
    assert(document.getSyntheticsCount > 0, "selected apply synthetics absent")
    val selected = document.getSyntheticsList.asScala.filter { synthetic =>
      synthetic.hasRange && synthetic.getRange.getStartLine == 18
    }
    assert(
      selected.exists(
        _.getTree.toString.contains("model/pool/Waiting.apply().")
      ),
      "callsite-correlated generated apply selection absent"
    )
  }
}
