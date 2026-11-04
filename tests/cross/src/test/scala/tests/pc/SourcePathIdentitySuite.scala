package tests.pc

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Paths
import java.time.Duration
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

import scala.reflect.io.PlainFile
import scala.reflect.io.VirtualFile

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.pc.MetalsSourceFile
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.internal.pc.ScalaPresentationCompiler
import scala.meta.internal.semanticdb.Diagnostic
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.pc.SourcePathMode
import scala.meta.pc.VirtualFileParams

class SourcePathIdentitySuite extends munit.FunSuite {
  test("physical-file-identity-preserves-editor-content") {
    val root = Files.createTempDirectory("source-buffer-identity-")
    val path = root.resolve("Unsaved model.scala")
    val disk = "object Model { val value = 1 }\n"
    val editor = "object Model { val value = 2 }\n"
    Files.write(path, disk.getBytes(UTF_8))
    try {
      val source = new MetalsSourceFile(path.toUri.toString, editor.toCharArray)
      assertEquals(
        source.file,
        new PlainFile(scala.reflect.io.Path(path.toFile))
      )
      assertEquals(source.content.mkString, editor)
      assertEquals(new String(Files.readAllBytes(path), UTF_8), disk)
    } finally {
      Files.deleteIfExists(path)
      Files.deleteIfExists(root)
    }
  }

  for (
    (filename, kind) <- List(
      "untitled:Buffer.scala" -> "untitled",
      "relative/Buffer.scala" -> "relative"
    )
  ) test(s"virtual-file-fallback-$kind") {
    val text = "object Buffer { val value = 42 }\n"
    val source = new MetalsSourceFile(filename, text.toCharArray)
    assert(source.file.isInstanceOf[VirtualFile])
    assertEquals(source.path, filename)
    assertEquals(source.content.mkString, text)
  }

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

  for {
    mode <- List(SourcePathMode.PRUNED, SourcePathMode.FULL)
    callerFirst <- List(true, false)
  } test(
    s"source-path-identity-${mode.toString.toLowerCase}-caller-first-$callerFirst"
  ) {
    val root = Files.createTempDirectory("source-path-identity-")
    // Spaces exercise URI decoding. The basename intentionally differs from
    // the declarations: that is what makes the source loader browse siblings.
    val models = root.resolve("Model declarations.scala")
    val calls = root.resolve("Caller.scala")
    Files.write(models, model.getBytes(UTF_8))
    Files.write(calls, caller.getBytes(UTF_8))
    val executor = Executors.newScheduledThreadPool(2)
    val compiler = new ScalaPresentationCompiler()
      .withConfiguration(PresentationCompilerConfigImpl(sourcePathMode = mode))
      .withExecutorService(executor)
      .withScheduledExecutorService(executor)
      .newInstance(
        "source-path-identity",
        List(
          Paths.get(
            classOf[
              Option[_]
            ].getProtectionDomain.getCodeSource.getLocation.toURI
          )
        ).asJava,
        List.empty[String].asJava,
        () => List(models, calls).asJava
      )
    def errors(document: TextDocument) =
      document.diagnostics.filter(_.severity == Diagnostic.Severity.ERROR)
    def query(text: String): TextDocument =
      TextDocument.parseFrom(
        compiler
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
        document.symbols.exists(_.symbol == "example/model/Generated.apply()."),
        "generated companion signature must survive re-query and cache eviction"
      )
    }
    try {
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
          compiler
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
      compiler.didClose(models.toUri)
      assertClean(query(model))
      // Exceed the five-unit cache using unsaved files; reopening the physical
      // declaration must not leave a second stale unit under the URI key.
      for (number <- 1 to 6) {
        val unsaved = root.resolve(s"Unsaved$number.scala")
        val text =
          s"package scratch; object Unsaved$number { def value = $number }"
        val document = TextDocument.parseFrom(
          compiler
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
    } finally {
      compiler.shutdown()
      executor.shutdownNow()
      Files.deleteIfExists(models)
      Files.deleteIfExists(calls)
      Files.deleteIfExists(root)
    }
  }
}
