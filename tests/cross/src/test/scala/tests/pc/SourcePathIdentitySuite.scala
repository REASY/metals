package tests.pc

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.time.Duration
import java.util.concurrent.TimeUnit

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.internal.semanticdb.Diagnostic
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.pc.SourcePathMode
import scala.meta.pc.VirtualFileParams

import tests.BasePCSuite

class SourcePathIdentitySuite extends BasePCSuite {
  protected def sourcePathMode: SourcePathMode = SourcePathMode.PRUNED

  override protected def config: PresentationCompilerConfigImpl =
    PresentationCompilerConfigImpl(sourcePathMode = sourcePathMode)

  override protected def sourceFiles: Seq[Path] =
    List("Model declarations.scala", "Caller.scala").map(tmp.resolve(_).toNIO)

  private val model =
    """package example.model
      |class Other(val value: String)
      |object Other { def create(value: String) = new Other(value) }
      |object Model {
      |  def apply(value: Int): Model = new Model(value.toString)
      |}
      |final case class Model(value: String) {
      |  def duplicate: Model = Model(value)
      |}
      |final case class Generated(value: String)
      |""".stripMargin
  private val caller =
    "package example; object Caller { def make = model.Model(\"sample\"); def generated = model.Generated(\"sample\") }"

  for (callerFirst <- List(true, false))
    test(
      s"source-path-identity-${sourcePathMode.toString.toLowerCase}-caller-first-$callerFirst"
    ) {
      presentationCompiler.restart()
      val root = tmp.toNIO
      // Spaces exercise URI decoding. The basename intentionally differs from
      // the declarations: that is what makes the source loader browse siblings.
      val models = root.resolve("Model declarations.scala")
      val calls = root.resolve("Caller.scala")
      Files.write(models, model.getBytes(UTF_8))
      Files.write(calls, caller.getBytes(UTF_8))
      def errors(document: TextDocument) =
        document.diagnostics.filter(_.severity == Diagnostic.Severity.ERROR)
      def query(text: String): TextDocument =
        TextDocument.parseFrom(
          presentationCompiler
            .semanticdbTextDocument(
              CompilerVirtualFileParams(
                models.toUri,
                text,
                shouldReturnDiagnostics = true
              )
            )
            .get(30, TimeUnit.SECONDS)
        )
      def assertClean(document: TextDocument): Unit = {
        assertEquals(errors(document), Nil)
        assert(
          document.symbols
            .exists(_.symbol == "example/model/Generated.apply()."),
          "generated companion signature must survive re-query and cache eviction"
        )
      }
      val inputs: List[VirtualFileParams] = List(
        CompilerVirtualFileParams(
          calls.toUri,
          caller,
          shouldReturnDiagnostics = true
        ),
        CompilerVirtualFileParams(
          models.toUri,
          model,
          shouldReturnDiagnostics = true
        )
      )
      val ordered = if (callerFirst) inputs else inputs.reverse
      val documents = TextDocuments
        .parseFrom(
          presentationCompiler
            .batchSemanticdbTextDocuments(
              ordered.asJava,
              Duration.ofSeconds(30)
            )
            .get(40, TimeUnit.SECONDS)
        )
        .documents
      assertEquals(documents.size, 2)
      documents.foreach(document => assertEquals(errors(document), Nil))
      val declaration = documents.find(_.uri == models.toUri.toString).get
      assert(
        declaration.symbols.exists(_.symbol == "example/model/Model.apply().")
      )
      assert(
        declaration.symbols.exists(
          _.symbol == "example/model/Generated.apply()."
        ),
        "generated companion signature must survive source-path promotion"
      )
      assert(
        declaration.synthetics.exists(_.range.exists(_.startLine == 7)),
        "explicit source bodies must not remain pruned"
      )

      // This error is only in the supplied buffer, not on disk. It must still
      // be diagnosed after the same file was loaded through the source path.
      val edited = model.replace(
        "def duplicate: Model = Model(value)",
        "def duplicate: Int = value"
      )
      val editedDocument = query(edited)
      assert(
        errors(editedDocument).nonEmpty,
        "unsaved real type error was hidden"
      )
      assertEquals(editedDocument.text, edited)
      assertClean(query(model))
      presentationCompiler.didClose(models.toUri)
      assertClean(query(model))
      // Exceed the five-unit cache using unsaved files; reopening the physical
      // declaration must not leave a second stale unit under the URI key.
      for (number <- 1 to 6) {
        val unsaved = root.resolve(s"Unsaved$number.scala")
        val text =
          s"package scratch; object Unsaved$number { def value = $number }"
        val document = TextDocument.parseFrom(
          presentationCompiler
            .semanticdbTextDocument(
              CompilerVirtualFileParams(
                unsaved.toUri,
                text,
                shouldReturnDiagnostics = true
              )
            )
            .get(30, TimeUnit.SECONDS)
        )
        assertEquals(errors(document), Nil)
        assert(!Files.exists(unsaved), "query must not save the editor buffer")
      }
      assertClean(query(model))
      assertEquals(new String(Files.readAllBytes(models), UTF_8), model)
    }
}

class SourcePathIdentityFullSuite extends SourcePathIdentitySuite {
  override protected def sourcePathMode: SourcePathMode = SourcePathMode.FULL
}
