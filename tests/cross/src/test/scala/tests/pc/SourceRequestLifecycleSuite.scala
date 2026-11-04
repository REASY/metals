package tests.pc

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.mtags.MD5
import scala.meta.internal.pc.MetalsGlobal
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.internal.pc.ScalaPresentationCompiler
import scala.meta.internal.pc.SemanticdbTextDocumentProvider
import scala.meta.internal.semanticdb.Diagnostic
import scala.meta.pc.SourcePathMode

class SourceRequestLifecycleSuite extends munit.FunSuite {
  private def withSource(
      text: String
  )(check: (MetalsGlobal, Path) => Unit): Unit = {
    val root = Files.createTempDirectory("source-request-lifecycle-")
    val path = root.resolve("Model.scala")
    Files.write(path, text.getBytes(UTF_8))
    val presentationCompiler = new ScalaPresentationCompiler()
      .withConfiguration(
        PresentationCompilerConfigImpl(
          emitDiagnostics = true,
          sourcePathMode = SourcePathMode.PRUNED
        )
      )
      .newInstance(
        "source-request-lifecycle",
        List(
          Paths.get(
            classOf[
              Option[_]
            ].getProtectionDomain.getCodeSource.getLocation.toURI
          )
        ).asJava,
        Nil.asJava,
        () => List(path).asJava
      )
      .asInstanceOf[ScalaPresentationCompiler]
    val compiler = presentationCompiler.newCompiler()
    try check(compiler, path)
    finally {
      compiler.askShutdown()
      presentationCompiler.shutdown()
      Files.deleteIfExists(path)
      Files.deleteIfExists(root)
    }
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
