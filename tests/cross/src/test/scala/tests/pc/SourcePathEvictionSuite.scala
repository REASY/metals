package tests.pc

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.TimeUnit

import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.internal.semanticdb.Diagnostic
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.pc.SourcePathMode

import tests.BasePCSuite

class SourcePathEvictionSuite extends BasePCSuite {
  protected def sourcePathMode: SourcePathMode = SourcePathMode.PRUNED

  override protected def config: PresentationCompilerConfigImpl =
    PresentationCompilerConfigImpl(sourcePathMode = sourcePathMode)

  override protected def sourceFiles: Seq[Path] =
    List("Model.scala", "Parser.scala", "A.scala", "B.scala", "C.scala",
      "D.scala", "E.scala")
      .map(tmp.resolve(_).toNIO)

  private val model =
    """package example
      |class Model {
      |  def parse(mode: Model.Mode): Model.Item = new Parser(mode).parse()
      |}
      |object Model {
      |  sealed trait Mode
      |  case object Relaxed extends Mode
      |  sealed trait Entity
      |  final case class Item(value: String) extends Entity
      |  object Item { def apply(value: Int): Item = new Item(value.toString) }
      |  final case class Packet(value: Item, mode: Mode) extends Entity
      |}
      |""".stripMargin

  for (pressure <- List(4, 5))
    test(
      s"nested-type-identity-${sourcePathMode.toString.toLowerCase}-$pressure-files"
    ) {
      presentationCompiler.restart()
      val root = tmp.toNIO
      val contents = List(
        "Model.scala" -> model,
        "Parser.scala" -> "package example; class Parser(mode: Model.Mode) { def parse(): Model.Item = Model.Item(\"sample\") }",
        "A.scala" -> "package example; object A { def make = Model.Item(\"a\") }",
        "B.scala" -> "package example; object B { def make = Model.Packet(Model.Item(\"b\"), Model.Relaxed) }",
        "C.scala" -> "package example; object C { def make = new Model().parse(Model.Relaxed) }",
        "D.scala" -> "package example; object D { def make = Model.Item(7) }",
        "E.scala" -> "package example; object E { def make = Model.Packet(Model.Item(\"e\"), Model.Relaxed) }"
      )
      contents.foreach { case (name, text) =>
        Files.write(root.resolve(name), text.getBytes(UTF_8))
      }
      def query(name: String, text: String): TextDocument =
        TextDocument.parseFrom(
          presentationCompiler
            .semanticdbTextDocument(
              CompilerVirtualFileParams(
                root.resolve(name).toUri,
                text,
                shouldReturnDiagnostics = true
              )
            )
            .get(30, TimeUnit.SECONDS)
        )
      def clean(document: TextDocument): Unit = {
        assertEquals(
          document.diagnostics.filter(_.severity == Diagnostic.Severity.ERROR),
          Nil
        )
        assert(
          document.symbols.nonEmpty,
          "empty document is not a clean result"
        )
      }
      val before = query("Model.scala", model)
      clean(before)
      for ((name, text) <- contents.drop(2).take(pressure))
        clean(query(name, text))
      val after = query("Model.scala", model)
      clean(after)
      // Generated local IDs may advance across typer runs. Preserve every
      // other field, including local signatures, and all global identities.
      def stableSymbols(document: TextDocument) = document.symbols.map {
        symbol =>
          if (symbol.symbol.matches("local[0-9]+")) symbol.withSymbol("local")
          else symbol
      }
      assertEquals(stableSymbols(after), stableSymbols(before))
      assertEquals(after.occurrences, before.occurrences)
      assertEquals(after.synthetics, before.synthetics)
      assertEquals(after.text, model)
      assert(after.symbols.exists(_.symbol == "example/Model.Packet.apply()."))
      val edited = model.replace(
        "def parse(mode: Model.Mode): Model.Item",
        "def parse(mode: Model.Mode): Int"
      )
      val erroneous = query("Model.scala", edited)
      assertEquals(erroneous.text, edited)
      assert(
        erroneous.diagnostics.exists(_.severity == Diagnostic.Severity.ERROR),
        "a genuine unsaved type error must survive cache eviction"
      )
      val restored = query("Model.scala", model)
      clean(restored)
      assertEquals(stableSymbols(restored), stableSymbols(before))
      assertEquals(restored.occurrences, before.occurrences)
      assertEquals(restored.synthetics, before.synthetics)
      assertEquals(
        new String(Files.readAllBytes(root.resolve("Model.scala")), UTF_8),
        model
      )
    }
}

class SourcePathEvictionFullSuite extends SourcePathEvictionSuite {
  override protected def sourcePathMode: SourcePathMode = SourcePathMode.FULL
}
