package tests.pc

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Paths
import java.time.Duration
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.internal.pc.ScalaPresentationCompiler
import scala.meta.internal.semanticdb.Diagnostic
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.internal.semanticdb.TextDocuments
import scala.meta.pc.SourcePathMode
import scala.meta.pc.VirtualFileParams

class CodehopSourcePathIdentitySuite extends munit.FunSuite {
  override def munitIgnore: Boolean =
    !scala.util.Properties.versionNumberString.startsWith("2.13.")

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

  private val cycleParser =
    """package example
      |final class CycleParser(input: String, mode: CycleUri.ParsingMode) {
      |  def run(): String = mode match {
      |    case CycleUri.ParsingMode.Strict => input.trim
      |    case CycleUri.ParsingMode.Relaxed => input
      |  }
      |}
      |""".stripMargin
  private val cycleUri =
    """package example
      |final class CycleUri(val raw: String) {
      |  def normalize(mode: CycleUri.ParsingMode = CycleUri.ParsingMode.Relaxed): String =
      |    new CycleParser(raw, mode).run()
      |}
      |object CycleUri {
      |  sealed trait ParsingMode
      |  object ParsingMode {
      |    case object Strict extends ParsingMode
      |    case object Relaxed extends ParsingMode
      |  }
      |  def parse(raw: String, mode: ParsingMode = ParsingMode.Relaxed): String =
      |    new CycleParser(raw, mode).run()
      |}
      |""".stripMargin

  for {
    mode <- List(SourcePathMode.PRUNED, SourcePathMode.FULL)
    callerFirst <- List(true, false)
    genuineError <- List(true, false)
    navigationPriming <- List(false, true)
  } test(
    s"nested-source-path-${mode.toString.toLowerCase}-caller-first-$callerFirst-real-error-$genuineError-primed-$navigationPriming"
  ) {
    val root = Files.createTempDirectory("nested-source-path-")
    val parserPath = root.resolve("CycleParser.scala")
    val uriPath = root.resolve("CycleUri.scala")
    Files.write(parserPath, cycleParser.getBytes(UTF_8))
    Files.write(uriPath, cycleUri.getBytes(UTF_8))
    val executor = Executors.newScheduledThreadPool(2)
    val compiler = new ScalaPresentationCompiler()
      .withConfiguration(PresentationCompilerConfigImpl(sourcePathMode = mode))
      .withExecutorService(executor)
      .withScheduledExecutorService(executor)
      .newInstance(
        "nested-source-path",
        List(
          Paths.get(
            classOf[
              Option[_]
            ].getProtectionDomain.getCodeSource.getLocation.toURI
          )
        ).asJava,
        List.empty[String].asJava,
        () => List(parserPath, uriPath).asJava
      )
    try {
      val target =
        if (genuineError)
          cycleUri.replace(
            "object CycleUri {",
            "object CycleUri {\n  def deliberatelyWrong: Int = \"not an Int\""
          )
        else cycleUri
      val parser = CompilerVirtualFileParams(
        parserPath.toUri,
        cycleParser,
        shouldReturnDiagnostics = true
      )
      val uri = CompilerVirtualFileParams(
        uriPath.toUri,
        target,
        shouldReturnDiagnostics = true
      )
      val params: List[VirtualFileParams] =
        if (callerFirst) List(parser, uri) else List(uri, parser)
      if (navigationPriming) {
        for (param <- params) {
          compiler
            .definition(
              CompilerOffsetParams(
                param.uri(),
                param.text(),
                param.text().indexOf("CycleUri")
              )
            )
            .get(40, TimeUnit.SECONDS)
        }
      }
      val documents = TextDocuments
        .parseFrom(
          compiler
            .batchSemanticdbTextDocuments(
              params.asJava,
              Duration.ofSeconds(30)
            )
            .get(40, TimeUnit.SECONDS)
        )
        .documents
      assertEquals(documents.map(_.uri), params.map(_.uri().toString()))
      assert(
        documents.forall(_.symbols.nonEmpty),
        "a requested document was lost"
      )
      val errors = documents.flatMap(
        _.diagnostics.filter(_.severity == Diagnostic.Severity.ERROR)
      )
      if (genuineError) {
        assertEquals(errors.size, 1)
        assert(errors.head.message.contains("required: Int"))
      } else assertEquals(errors, Nil)
      // The batch may isolate a previously source-loaded document. The
      // constructor endpoint must observe the same healthy compilation.
      val constructors = compiler
        .scalaConstructorEvidence(uri)
        .get(40, TimeUnit.SECONDS)
      assertEquals(
        constructors.status,
        if (genuineError) "UNAVAILABLE" else "AVAILABLE",
        clues(constructors.reason)
      )
      if (genuineError) assertEquals(constructors.reason, "COMPILER_ERROR")
    } finally {
      compiler.shutdown()
      executor.shutdownNow()
      Files.deleteIfExists(parserPath)
      Files.deleteIfExists(uriPath)
      Files.deleteIfExists(root)
    }
  }

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
