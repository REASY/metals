package tests

import scala.concurrent.Future

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.SemanticdbTextDocumentsParams
import scala.meta.internal.metals.config.ReferenceProviderConfig
import scala.meta.internal.metals.config.UserConfiguration
import scala.meta.internal.metals.config.WorkspaceSymbolProviderConfig

class ScalaCallableEvidenceLifecycleLspSuite
    extends BaseLspSuite("scala-callable-lifecycle") {
  override def userConfig: UserConfiguration = super.userConfig.copy(
    buildOnFocus = false,
    buildOnChange = false,
    workspaceSymbolProvider = WorkspaceSymbolProviderConfig.MBT,
    referenceProvider = ReferenceProviderConfig.MBT,
  )
  for (kind <- List("full", "pruned", "outline"))
    testLSP(s"selected-callables-after-navigation-$kind") {
      cleanWorkspace()
      val caller = "a/src/main/scala/model/pool/Caller.scala"
      val states = "a/src/main/scala/model/pool/States.scala"
      val files = List(
        caller,
        states,
        "a/src/main/scala/model/Flow.scala",
        "a/src/main/scala/alternate/Flow.scala",
      )
      for {
        _ <- initialize(
          """|/metals.json
             |{"a":{"scalaVersion":"2.13.18"}}
             |/a/src/main/scala/model/pool/Caller.scala
             |package model.pool
             |
             |import model.Flow.RequestContext
             |import java.util.concurrent._
             |import scala.concurrent.duration._
             |
             |object Caller {
             |  def waiting(request: RequestContext): Waiting = Waiting(request, false)
             |  def shadow(request: alternate.Flow.RequestContext): ShadowWaiting = ShadowWaiting(request, false)
             |  def overloaded(request: RequestContext): Overloaded = Overloaded(request)
             |  def retained(request: RequestContext): RetainedOverloaded = RetainedOverloaded(request)
             |}
             |/a/src/main/scala/model/pool/States.scala
             |package model.pool
             |
             |import model.Flow.RequestContext
             |import java.util.concurrent._
             |import scala.concurrent.duration._
             |
             |final case class Waiting(ongoingRequest: RequestContext, done: Boolean)
             |final case class ShadowWaiting(ongoingRequest: alternate.Flow.RequestContext, done: Boolean)
             |final case class Overloaded(ongoingRequest: RequestContext)
             |object Overloaded {
             |  def apply(ongoingRequest: alternate.Flow.RequestContext): Overloaded =
             |    new Overloaded(model.Flow.RequestContext(ongoingRequest.value.toString))
             |}
             |
             |final case class RetainedOverloaded(ongoingRequest: RequestContext)
             |object RetainedOverloaded {
             |  def apply(ongoingRequest: alternate.Flow.RequestContext, ignored: Boolean): RetainedOverloaded =
             |    new RetainedOverloaded(model.Flow.RequestContext(ongoingRequest.value.toString))
             |}
             |/a/src/main/scala/model/Flow.scala
             |package model
             |
             |object Flow {
             |  final case class RequestContext(value: String)
             |}
             |/a/src/main/scala/alternate/Flow.scala
             |package alternate
             |
             |object Flow {
             |  final case class RequestContext(value: Int)
             |}
             |""".stripMargin
        )
        _ <- List(files(2), files(3), states, caller).foldLeft(
          Future.successful(())
        ) { (previous, file) =>
          previous.flatMap(_ => server.didOpen(file))
        }
        _ <- server.semanticdbTextDocuments(files)
        // BuildTargetClasses runs this public-symbol-only compiler request after
        // a build; it must not replace the caller's full body in the shared PC.
        _ <-
          if (kind == "pruned")
            server.headServer.compilers
              .batchSemanticdbTextDocuments(
                Seq(server.toPath(caller)),
                scala.meta.internal.metals.EmptyCancelToken,
                java.time.Duration.ofSeconds(15),
                useFallbackCompiler = false,
                requireBuildTargetClasspath = true,
                shouldPruneSemanticdb = true,
              )
              .map(_ => ())
          else if (kind == "outline") {
            val pc = server.headServer.compilers
              .loadCompiler(server.toPath(caller))
              .get
            val input = scala.meta.internal.metals.CompilerVirtualFileParams(
              server.toPath(caller).toURI,
              server.textContents(caller),
            )
            pc.semanticdbTextDocument(
              input.copy(outlineFiles =
                java.util.Optional.of(
                  scala.meta.internal.metals.OutlineFiles(
                    List[scala.meta.pc.VirtualFileParams](input).asJava
                  )
                )
              )
            ).asScala
              .map(_ => ())
          } else Future.unit
        _ <- List(
          ("@@Waiting(request, false)", "Waiting(@@request, false)"),
          (
            "@@ShadowWaiting(request, false)",
            "ShadowWaiting(@@request, false)",
          ),
          ("@@Overloaded(request)", "Overloaded(@@request)"),
        ).foldLeft(Future.successful(())) {
          case (previous, (query, signatureQuery)) =>
            previous.flatMap { _ =>
              val position =
                server.textDocumentPositionParams(caller, signatureQuery)
              for {
                _ <- server.definitionSubstringQuery(caller, query)
                hoverPosition = server.textDocumentPositionParams(caller, query)
                _ <- server.fullServer
                  .hover(
                    new scala.meta.internal.metals.HoverExtParams(
                      hoverPosition.getTextDocument,
                      hoverPosition.getPosition,
                    )
                  )
                  .asScala
                _ <- server.fullServer
                  .signatureHelp(
                    new org.eclipse.lsp4j.SignatureHelpParams(
                      position.getTextDocument,
                      position.getPosition,
                    )
                  )
                  .asScala
              } yield ()
            }
        }
        _ <- server.definitionSubstringQuery(
          states,
          "Waiting(ongoingRequest: @@RequestContext",
        )
        formalPosition = server.textDocumentPositionParams(
          states,
          "Waiting(ongoingRequest: @@RequestContext",
        )
        _ <- server.fullServer
          .hover(
            new scala.meta.internal.metals.HoverExtParams(
              formalPosition.getTextDocument,
              formalPosition.getPosition,
            )
          )
          .asScala
        _ <- server.semanticdbTextDocuments(files)
        result <- server.fullServer
          .scalaCallableEvidence(
            SemanticdbTextDocumentsParams(
              files.map(file => server.toPath(file).toURI.toString).asJava
            )
          )
          .asScala
      } yield {
        val document = result.documents.asScala
          .find(_.uri == server.toPath(caller).toURI.toString)
          .get
        assertEquals(document.status, "AVAILABLE")
        assertEquals(document.selections.size(), 4)
        val expected = Map(
          "model/pool/Waiting.apply()." -> "model/Flow.RequestContext#",
          "model/pool/ShadowWaiting.apply()." -> "alternate/Flow.RequestContext#",
          "model/pool/Overloaded.apply(+1)." -> "model/Flow.RequestContext#",
          "model/pool/RetainedOverloaded.apply(+1)." -> "model/Flow.RequestContext#",
        )
        assertEquals(
          document.selections.asScala.map(_.symbol).toSet,
          expected.keySet,
        )
        val signatures = result.documents.asScala.flatMap(_.callables.asScala)
        for ((symbol, formalType) <- expected) {
          val matching = signatures.filter(_.symbol == symbol)
          assertEquals(matching.size, 1)
          assertEquals(matching.head.status, "AVAILABLE")
          assertEquals(
            matching.head.parameterLists
              .get(0)
              .get(0)
              .tpe
              .get("symbol")
              .getAsString,
            formalType,
          )
        }
      }
    }

}
