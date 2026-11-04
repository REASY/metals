package scala.meta.internal.metals.mbt

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.util.jar.JarEntry
import java.util.jar.JarOutputStream
import java.{util => ju}
import javax.tools.DiagnosticCollector
import javax.tools.JavaFileManager
import javax.tools.JavaFileObject
import javax.tools.StandardLocation.CLASS_PATH
import javax.tools.ToolProvider

import scala.collection.parallel.mutable.ParArray
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

import scala.meta.internal.jpc.SourceJavaFileObject
import scala.meta.internal.metals.Directories
import scala.meta.internal.metals.LoggerReportContext
import scala.meta.internal.metals.MetalsEnrichments.XtensionAbsolutePathBuffers
import scala.meta.internal.metals.ReportContext
import scala.meta.internal.metals.Sleeper
import scala.meta.internal.metals.Time
import scala.meta.internal.metals.config.TurbineRecompileDelayConfig
import scala.meta.internal.metals.config.UserConfiguration
import scala.meta.io.AbsolutePath
import scala.meta.pc.ProgressBars

import com.google.turbine.bytecode.ClassReader
import com.google.turbine.diag.SourceFile
import tests.BaseSuite

class TurbineDirectoryClasspathSuite extends BaseSuite {
  implicit private val ec: ExecutionContext = ExecutionContext.global
  implicit private val rc: ReportContext = LoggerReportContext

  private def compiler = newCompiler(Seq.empty)

  private def newCompiler(sources: Seq[SourceFile]) = new TurbineCompiler[Unit](
    allCompilationUnits = () =>
      if (sources.isEmpty) ParArray.empty[Unit] else ParArray(()),
    parseUnit = _ => sources,
    classpath = () => Seq.empty,
    progressBars = ProgressBars.EMPTY,
    turbineRecompileDelay = () => TurbineRecompileDelayConfig.testing,
    listProtoJavaOutlinesForPackage = _ => Iterator.empty,
    sleeper = Sleeper.TestingSleeper,
    onIndexingDone = () => (),
    onNewProjectClasspath = _ => (),
  )

  private def withRoot(fn: Path => Unit): Unit = {
    val root = Files.createTempDirectory("turbine-directory-classpath")
    try fn(root)
    finally AbsolutePath(root).deleteRecursively()
  }

  private def compile(root: Path, name: String, value: Int): Path = {
    val out = Files.createDirectories(root.resolve("classes"))
    val source = root.resolve(name.replace('.', '/') + ".java")
    Files.createDirectories(source.getParent())
    val pkg = name.substring(0, name.lastIndexOf('.'))
    val simple = name.substring(name.lastIndexOf('.') + 1)
    Files.write(
      source,
      s"package $pkg; public class $simple { public static final int VALUE = $value; }"
        .getBytes(UTF_8),
    )
    assertEquals(
      ToolProvider
        .getSystemJavaCompiler()
        .run(
          null,
          null,
          null,
          "--release",
          "17",
          "-classpath",
          "",
          "-d",
          out.toString(),
          source.toString(),
        ),
      0,
    )
    out
  }

  private def jar(root: Path, classes: Path, name: String): Path = {
    val path = root.resolve("library.jar")
    val stream = new JarOutputStream(Files.newOutputStream(path))
    try {
      val entry = name.replace('.', '/') + ".class"
      stream.putNextEntry(new JarEntry(entry))
      Files.copy(classes.resolve(entry), stream)
      stream.closeEntry()
    } finally stream.close()
    path
  }

  private def withManager(
      paths: Seq[Path],
      owner: TurbineCompiler[Unit] = compiler,
  )(fn: JavaFileManager => Unit): Unit = {
    val standard = ToolProvider
      .getSystemJavaCompiler()
      .getStandardFileManager(null, null, null)
    // Match JavaPruneCompiler's explicit, target-scoped -classpath, including empty.
    standard.setLocationFromPaths(CLASS_PATH, paths.asJava)
    val manager = owner.createFileManager(standard, paths.asJava)
    try fn(manager)
    finally manager.close()
  }

  private def list(
      manager: JavaFileManager,
      pkg: String = "example",
      recurse: Boolean = false,
      kind: JavaFileObject.Kind = JavaFileObject.Kind.CLASS,
  ): Seq[JavaFileObject] =
    manager.list(CLASS_PATH, pkg, ju.EnumSet.of(kind), recurse).asScala.toSeq

  private def bytes(file: JavaFileObject): Seq[Byte] = {
    val stream = file.openInputStream()
    try stream.readAllBytes().toSeq
    finally stream.close()
  }

  test("target-directory-and-jar-controls") {
    withRoot { root =>
      val classes = compile(root, "example.Library", 1)
      val archive = jar(root, classes, "example.Library")
      for (path <- Seq(classes, archive)) withManager(Seq(path)) { manager =>
        val found = list(manager)
        assertEquals(
          found.map(manager.inferBinaryName(CLASS_PATH, _)),
          Seq("example.Library"),
        )
        assertEquals(
          bytes(found.head),
          Files.readAllBytes(classes.resolve("example/Library.class")).toSeq,
        )
      }
    }
  }

  test("partial-workspace-outline-does-not-erase-target-return-type") {
    withRoot { root =>
      val classes = compile(root, "example.View", 1)
      val bridge =
        "package example; public class Bridge { public View toScala() { return null; } }"
      val source = root.resolve("example/Bridge.java")
      Files.write(source, bridge.getBytes(UTF_8))
      assertEquals(
        ToolProvider
          .getSystemJavaCompiler()
          .run(
            null,
            null,
            null,
            "--release",
            "17",
            "-classpath",
            classes.toString(),
            "-d",
            classes.toString(),
            source.toString(),
          ),
        0,
      )
      def descriptor(bytes: Array[Byte]): String =
        ClassReader
          .read("example/Bridge.class", bytes)
          .methods()
          .asScala
          .find(_.name() == "toScala")
          .getOrElse(fail("missing toScala method"))
          .descriptor()
      assertEquals(
        descriptor(Files.readAllBytes(classes.resolve("example/Bridge.class"))),
        "()Lexample/View;",
      )
      // The workspace-global outline compiler has no target-local View input.
      val owner = newCompiler(Seq(new SourceFile(source.toString(), bridge)))
      val result = Await.result(owner.compileNow(), 10.seconds)
      val outline = Option(result.lowered.bytes().get("example/Bridge"))
      assertEquals(outline.map(descriptor), None)
      assertEquals(result.sourceFallbacks.map(_.text), Seq(bridge))
      withManager(Seq(classes), owner) { manager =>
        val selected = list(manager)
          .find(file =>
            manager.inferBinaryName(CLASS_PATH, file) == "example.Bridge"
          )
          .getOrElse(fail("missing selected Bridge"))
        assertEquals(descriptor(bytes(selected).toArray), "()Lexample/View;")
      }
    }
  }

  test("partial-outline-source-is-typed-in-target-and-keeps-edits") {
    withRoot { root =>
      val classes = compile(root, "example.View", 1)
      val stale = root.resolve("example/Bridge.java")
      Files.write(
        stale,
        "package example; public class Bridge { public Object oldName() { return null; } }"
          .getBytes(UTF_8),
      )
      assertEquals(
        ToolProvider
          .getSystemJavaCompiler()
          .run(
            null,
            null,
            null,
            "--release",
            "17",
            "-classpath",
            classes.toString(),
            "-d",
            classes.toString(),
            stale.toString(),
          ),
        0,
      )
      val current =
        "package example; public class Bridge { public View toScala() { return null; } }"
      val owner = newCompiler(Seq(new SourceFile(stale.toString(), current)))
      Await.result(owner.compileNow(), 10.seconds)
      val caller = SourceJavaFileObject.make(
        "package example; public class Caller { View run(Bridge b) { return b.toScala(); } }",
        root.resolve("example/Caller.java").toUri(),
      )
      def check(paths: Seq[Path], expected: Boolean): Unit = {
        withManager(paths, owner) { manager =>
          val diagnostics = new DiagnosticCollector[JavaFileObject]()
          val out = Files.createTempDirectory(root, "javac-out")
          val task = ToolProvider
            .getSystemJavaCompiler()
            .getTask(
              null,
              manager,
              diagnostics,
              Seq(
                "--release",
                "17",
                "-proc:none",
                "-Xprefer:source",
                "-sourcepath",
                "",
                "-d",
                out.toString(),
              ).asJava,
              null,
              Seq(caller).asJava,
            )
          assertEquals(
            task.call().booleanValue(),
            expected,
            clues(diagnostics.getDiagnostics()),
          )
        }
      }
      check(Seq(classes), expected = true)
      check(Seq.empty, expected = false)
      owner.onDidDelete(Seq("example/Bridge"), stale.toUri().toString())
      check(Seq(classes), expected = false)
    }
  }

  test("partial-outlines-invalidate-class-only-cache") {
    withRoot { root =>
      val owner = newCompiler(
        Seq(
          new SourceFile(
            root.resolve("Bridge.java").toString(),
            "package example; public class Bridge { public Missing toScala() { return null; } }",
          )
        )
      )
      val result = Await.result(owner.compileNow(), 10.seconds)
      assert(result.sourceFallbacks.nonEmpty)
      val path = AbsolutePath(root).resolve(Directories.turbineCache).toNIO
      Files.createDirectories(path.getParent())
      Files.write(path, Array[Byte](1, 2, 3))
      val cache = new TurbineCache(
        AbsolutePath(root),
        () =>
          UserConfiguration(
            javaTurbineCache = true,
            javaTurbineRecompileDelay = TurbineRecompileDelayConfig.testing,
          ),
        Time.system,
      )
      cache.writeCache(result)
      assert(
        !Files.exists(path),
        "a partial compile must not preserve or publish a class-only cache",
      )
    }
  }

  test("target-classpath-order-and-binary-name-deduplication") {
    withRoot { root =>
      val first = compile(root.resolve("first"), "example.Library", 1)
      val second = compile(root.resolve("second"), "example.Library", 2)
      val archive = jar(root, second, "example.Library")
      for (
        paths <- Seq(
          Seq(first, archive),
          Seq(archive, first),
          Seq(first, second),
          Seq(first, first),
        )
      ) {
        withManager(paths) { manager =>
          val found = list(manager)
          assertEquals(found.size, 1)
          val expected = if (paths.head == archive) second else first
          assertEquals(
            bytes(found.head),
            Files.readAllBytes(expected.resolve("example/Library.class")).toSeq,
          )
        }
      }
    }
  }

  test("target-isolation-and-empty-classpath") {
    withRoot { root =>
      val first = compile(root.resolve("first"), "example.First", 1)
      val second = compile(root.resolve("second"), "example.Second", 2)
      val shared = compiler
      withManager(Seq(first), shared) { a =>
        withManager(Seq(second), shared) { b =>
          assertEquals(
            list(a).map(a.inferBinaryName(CLASS_PATH, _)),
            Seq("example.First"),
          )
          assertEquals(
            list(b).map(b.inferBinaryName(CLASS_PATH, _)),
            Seq("example.Second"),
          )
        }
      }
      withManager(Seq.empty) { manager =>
        assertEquals(list(manager), Seq.empty)
        assertEquals(list(manager, "scala"), Seq.empty)
      }
    }
  }

  test("directory-list-respects-kinds-and-recursion") {
    withRoot { root =>
      val classes = compile(root, "example.Library", 1)
      compile(root, "example.nested.Child", 2)
      withManager(Seq(classes)) { manager =>
        assertEquals(
          list(manager).map(manager.inferBinaryName(CLASS_PATH, _)),
          Seq("example.Library"),
        )
        assertEquals(
          list(manager, recurse = true)
            .map(manager.inferBinaryName(CLASS_PATH, _))
            .toSet,
          Set("example.Library", "example.nested.Child"),
        )
        assertEquals(
          list(manager, kind = JavaFileObject.Kind.SOURCE),
          Seq.empty,
        )
      }
    }
  }

  test("deleted-source-does-not-reappear-from-directory-or-jar") {
    withRoot { root =>
      val classes = compile(root, "example.Library", 1)
      val archive = jar(root, classes, "example.Library")
      for (path <- Seq(classes, archive)) {
        val owner = compiler
        withManager(Seq(path), owner) { manager =>
          assertEquals(list(manager).size, 1)
          owner.onDidDelete(
            Seq("example/Library"),
            root.resolve("example/Library.java").toUri().toString(),
          )
          assertEquals(list(manager), Seq.empty)
        }
      }
    }
  }

  test("target-classpath-precedes-workspace-outline-with-one-binary-name") {
    withRoot { root =>
      val classes = compile(root, "example.Library", 1)
      val archive = jar(root, classes, "example.Library")
      val owner = newCompiler(
        Seq(
          new SourceFile(
            "example/Library.java",
            "package example; public class Library { public static final int VALUE = 3; }",
          )
        )
      )
      val result = Await.result(owner.compileNow(), 10.seconds)
      val outline = Option(result.lowered.bytes().get("example/Library"))
        .getOrElse(fail("missing workspace outline"))
        .toSeq
      val expected =
        Files.readAllBytes(classes.resolve("example/Library.class")).toSeq
      assertNotEquals(
        outline,
        expected,
      )
      withManager(Seq(classes, archive), owner) { manager =>
        val found = list(manager)
        assertEquals(found.size, 1)
        assertEquals(bytes(found.head), expected)
        assertEquals(
          list(manager, kind = JavaFileObject.Kind.SOURCE),
          Seq.empty,
        )
      }
    }
  }

  test("fresh-target-manager-observes-recompiled-and-removed-classfiles") {
    withRoot { root =>
      val classes = compile(root, "example.Library", 1)
      val original =
        Files.readAllBytes(classes.resolve("example/Library.class")).toSeq
      withManager(Seq(classes)) { manager =>
        assertEquals(bytes(list(manager).head), original)
      }
      compile(root, "example.Library", 2)
      val updated =
        Files.readAllBytes(classes.resolve("example/Library.class")).toSeq
      assertNotEquals(updated, original)
      val source = root.resolve("example/Library.java")
      val sourceBefore = Files.readAllBytes(source).toSeq
      withManager(Seq(classes)) { manager =>
        assertEquals(bytes(list(manager).head), updated)
      }
      assertEquals(Files.readAllBytes(source).toSeq, sourceBefore)
      Files.delete(classes.resolve("example/Library.class"))
      withManager(Seq(classes)) { manager =>
        assertEquals(list(manager), Seq.empty)
      }
    }
  }
}
