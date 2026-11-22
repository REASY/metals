package tests.pc

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path

import scala.reflect.io.PlainFile
import scala.reflect.io.VirtualFile

import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.mtags.MD5
import scala.meta.internal.pc.MetalsGlobal
import scala.meta.internal.pc.MetalsSourceFile
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.internal.pc.ScalaPresentationCompiler
import scala.meta.internal.pc.SemanticdbTextDocumentProvider
import scala.meta.internal.semanticdb.Diagnostic
import scala.meta.pc.SourcePathMode

import tests.BasePCSuite

class SourceRequestLifecycleSuite extends BasePCSuite {
  override protected def config: PresentationCompilerConfigImpl =
    PresentationCompilerConfigImpl(
      emitDiagnostics = true,
      sourcePathMode = SourcePathMode.PRUNED
    )

  override protected def sourceFiles: Seq[Path] =
    (List("Model.scala") ++ (0 to 6).map(n => s"Unit$n.scala"))
      .map(tmp.resolve(_).toNIO)
      .filter(Files.exists(_))

  private def withCompiler(check: MetalsGlobal => Unit): Unit = {
    val compiler =
      presentationCompiler.asInstanceOf[ScalaPresentationCompiler].newCompiler()
    try check(compiler)
    finally compiler.askShutdown()
  }

  private def withSource(
      text: String
  )(check: (MetalsGlobal, Path) => Unit): Unit = {
    val path = tmp.resolve("Model.scala").toNIO
    Files.write(path, text.getBytes(UTF_8))
    try withCompiler(compiler => check(compiler, path))
    finally Files.deleteIfExists(path)
  }

  test("physical-file-identity-preserves-editor-content") {
    val root = tmp.toNIO
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

  test("new-sources-and-evicted-reentry-refresh-typer-epoch") {
    val root = tmp.toNIO
    val sources = (0 to 6).map { number =>
      val path = root.resolve(s"Unit$number.scala")
      val text = s"package example; object Unit$number { val value = $number }"
      Files.write(path, text.getBytes(UTF_8))
      path -> text
    }
    try
      withCompiler { global =>
        def add(number: Int): Unit = {
          val (path, text) = sources(number)
          global.addCompilationUnit(text, path.toUri.toString, None)
        }
        for (number <- sources.indices) {
          val previousRun = global.currentRunId
          add(number)
          assertEquals(
            global.currentRunId,
            previousRun + 1,
            "registering a new source must advance the epoch exactly once"
          )
        }
        val beforeReentry = global.currentRunId
        add(0)
        assert(
          global.currentRunId > beforeReentry,
          "re-entering the evicted source must advance the epoch"
        )
        val afterReentry = global.currentRunId
        add(0)
        assertEquals(
          global.currentRunId,
          afterReentry,
          "an unchanged retained source must preserve the epoch"
        )
      }
    finally sources.foreach { case (path, _) => Files.deleteIfExists(path) }
  }

  test("cursor-moves-and-hover-preserve-epoch-but-edits-reset-it") {
    val text = "object Model { val marker = \"_CURSOR_\"; val value = 1 }"
    withSource(text) { (compiler, path) =>
      val uri = path.toUri.toString
      compiler.addCompilationUnit(text, uri, None)
      val initial = compiler.currentRunId
      compiler.addCompilationUnit(text, uri, Some(text.indexOf("value")))
      assertEquals(compiler.currentRunId, initial)
      compiler.addCompilationUnit(text, uri, Some(text.indexOf("value") + 2))
      assertEquals(compiler.currentRunId, initial)
      compiler.addCompilationUnit(text, uri, None)
      assertEquals(compiler.currentRunId, initial)
      val edited = text.replace("= 1", "= 2")
      compiler.addCompilationUnit(edited, uri, Some(edited.indexOf("value")))
      assert(compiler.currentRunId > initial)
      val afterEdit = compiler.currentRunId
      compiler.addCompilationUnit(
        edited.replace("_CURSOR_", "literal"),
        uri,
        None
      )
      assert(compiler.currentRunId > afterEdit)
    }
  }

  test("edited-source-unlinks-stale-top-levels-from-previous-run") {
    val text = "class OldName { }"
    withSource(text) { (compiler, path) =>
      import compiler._
      val uri = path.toUri.toString
      val unit = addCompilationUnit(text, uri, None)
      val owner = rootMirror.EmptyPackageClass
      val old = owner.newClassSymbol(TypeName("OldName"))
      val start = text.indexOf("OldName")
      old.setPos(rangePos(unit.source, start, start, start + "OldName".length))
      old.setInfo(ClassInfoType(List(definitions.ObjectTpe), newScope, old))
      owner.info.decls.enter(old)
      currentRun.symSource(old) = unit.source.file
      assertEquals(owner.info.decls.lookup(TypeName("OldName")), old)
      addCompilationUnit(text.replace("OldName", "NewName"), uri, None)
      assertEquals(owner.info.decls.lookup(TypeName("OldName")), NoSymbol)
    }
  }

  test("rename-cleans-top-levels-after-an-intervening-file-edit") {
    val text =
      "package example; class OldName {}; object OldName {}; class Kept {}"
    withSource(text) { (compiler, path) =>
      import compiler.{assert => _, _}
      val provider = new SemanticdbTextDocumentProvider(compiler, Nil)
      def query(file: Path, code: String): Unit = {
        val document = provider.textDocument(file.toUri, code)
        assertEquals(
          document.diagnostics.filter(_.severity == Diagnostic.Severity.ERROR),
          Nil
        )
        assert(document.symbols.nonEmpty)
      }
      query(path, text)
      val oldClass = rootMirror.staticClass("example.OldName")
      val oldModule = rootMirror.staticModule("example.OldName")
      val owner = oldClass.owner
      val kept = rootMirror.staticClass("example.Kept")
      val other = path.resolveSibling("Unrelated.scala")
      val otherText = "package example; class Unrelated { val value = 1 }"
      query(other, otherText)
      val epoch = currentRunId
      query(other, otherText.replace("= 1", "= 2"))
      assert(currentRunId > epoch)
      // Keep exercising the cached-tree fallback even though Metals now carries
      // retained source ownership across its own epoch refreshes.
      currentRun.symSource.keys
        .filter { symbol =>
          currentRun.symSource(symbol) == oldClass.pos.source.file
        }
        .toList
        .foreach(currentRun.symSource.remove)
      assert(!currentRun.symSource.values.exists(_ == oldClass.pos.source.file))
      val renamed = text.replace("OldName", "NewName")
      addCompilationUnit(renamed, path.toUri.toString, None)
      assertEquals(owner.info.decls.lookup(oldClass.name), NoSymbol)
      assertEquals(owner.info.decls.lookup(oldModule.name), NoSymbol)
      assertEquals(owner.info.decls.lookup(kept.name), kept)
      query(path, renamed)
      assert(rootMirror.staticClass("example.NewName") != NoSymbol)
      assert(rootMirror.staticModule("example.NewName") != NoSymbol)
      assert(rootMirror.staticClass("example.Unrelated") != NoSymbol)
      assertEquals(new String(Files.readAllBytes(path), UTF_8), text)
      assert(!Files.exists(other))
    }
  }

  test("pruned-input-cache-is-released-with-unsaved-error-evidence-retained") {
    val text = "object Model { def value: Int = 1 }"
    withSource(text) { (compiler, path) =>
      val provider = new SemanticdbTextDocumentProvider(compiler, Nil)
      val first = provider.textDocument(
        CompilerVirtualFileParams(
          path.toUri,
          text,
          shouldReturnDiagnostics = true,
          shouldPruneSemanticdb = true
        )
      )
      assert(first.symbols.nonEmpty)
      val unsaved = path.getParent.resolve("Unsaved.scala")
      val edited = "object Unsaved { def value: MissingType = ??? }"
      val second = provider.textDocument(
        CompilerVirtualFileParams(
          unsaved.toUri,
          edited,
          shouldReturnDiagnostics = true,
          shouldPruneSemanticdb = true
        )
      )
      assert(second.diagnostics.exists(_.severity == Diagnostic.Severity.ERROR))
      assertEquals(second.md5, MD5.compute(edited))
      assert(!Files.exists(unsaved))
      assertEquals(new String(Files.readAllBytes(path), UTF_8), text)
      assertEquals(compiler.semanticdbOps.gSourceFileInputCache.size, 0)
    }
  }
}
