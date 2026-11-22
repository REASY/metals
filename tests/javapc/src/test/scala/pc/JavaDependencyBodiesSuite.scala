package pc

import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.security.MessageDigest
import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletionStage
import java.util.concurrent.ExecutionException
import java.util.concurrent.TimeUnit
import java.util.jar.Attributes
import java.util.jar.JarEntry
import java.util.jar.JarOutputStream
import java.util.jar.Manifest
import javax.tools.ToolProvider

import scala.jdk.CollectionConverters._

import scala.meta.internal.jpc.JavaPresentationCompiler
import scala.meta.internal.jpc.JavaReceiverTarget
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.pc.CancelToken
import scala.meta.pc.VirtualFileParams

import com.google.gson.JsonObject
import coursierapi.Fetch
import tests.pc.BaseJavaPCSuite

class JavaDependencyBodiesSuite extends BaseJavaPCSuite {
  override protected def javacOptions: List[String] =
    List("--release", "17", "-proc:none")

  private lazy val library: Path = buildLibrary()

  private def buildLibrary(extraMembers: String = ""): Path = {
    val root = Files.createTempDirectory("dependency-body-fixture")
    val source = root.resolve("Api.java")
    val classes = Files.createDirectory(root.resolve("classes"))
    Files.writeString(
      source,
      s"""package fixture.dep;
         |public final class Api {
         |  public interface Service<T> { T apply(T value); }
         |  public abstract static class Base<T> implements Service<T> {}
         |  public static final class First extends Base<String> {
         |    public String apply(String value) { return value; }
         |  }
         |  public static final class Second implements Service<String> {
         |    public String apply(String value) { return value; }
         |  }
         |  public static final class NumberOnly implements Service<Integer> {
         |    public Integer apply(Integer value) { return value; }
         |  }
         |  public static final class Generic<T> implements Service<T> {
         |    public T apply(T value) { return value; }
         |  }
         |  public static final class NativeService implements Service<String> {
         |    public native String apply(String value);
         |  }
         |  public static final class Unrelated {
         |    public String apply(String value) { return value; }
         |  }
         |  public interface Missing { String missing(); }
         |  public interface Overloaded { String apply(int x); String apply(String x); }
         |  public static final class Overloads implements Overloaded {
         |    public String apply(int x) { return ""; }
         |    public String apply(String x) { return x; }
         |  }
         |  $extraMembers
         |}
         |""".stripMargin,
    )
    assertEquals(
      ToolProvider.getSystemJavaCompiler.run(
        null,
        null,
        null,
        "--release",
        "17",
        "-proc:none",
        "-d",
        classes.toString,
        source.toString,
      ),
      0,
    )
    val jar = root.resolve("dependency-1.0.0.jar")
    val out = new JarOutputStream(Files.newOutputStream(jar))
    val files = Files.walk(classes)
    try {
      for (
        file <- files.toArray
          .map(_.asInstanceOf[Path])
          .filter(Files.isRegularFile(_))
          .sortBy(_.toString)
      ) {
        val entry = new JarEntry(
          classes.relativize(file).toString.replace('\\', '/')
        )
        entry.setTime(0)
        out.putNextEntry(entry)
        Files.copy(file, out)
        out.closeEntry()
      }
    } finally {
      files.close()
      out.close()
    }
    jar
  }

  override protected def extraLibraries(fetch: Fetch): Seq[Path] = Seq(library)

  private lazy val multiReleaseLibrary: Path = {
    val root = Files.createTempDirectory("dependency-mr-fixture")
    val manifest = new Manifest()
    manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
    manifest.getMainAttributes.put(Attributes.Name.MULTI_RELEASE, "true")
    val jar = root.resolve("multi-release.jar")
    val out = new JarOutputStream(Files.newOutputStream(jar), manifest)
    try {
      for (release <- List(8, 17, 21)) {
        val dir = Files.createDirectory(root.resolve(s"v$release"))
        val source = dir.resolve("Api.java")
        val classes = Files.createDirectory(dir.resolve("classes"))
        // Same binary First has a different inherited physical body per
        // release. FutureOnly exists solely in the 21 versioned tree.
        val future =
          if (release == 21)
            """public static final class FutureOnly implements Service<String> {
              | public String apply(String value) { return value; }
              |}""".stripMargin
          else ""
        Files.writeString(
          source,
          s"""package fixture.dep;
             |public final class Api {
             | public interface Service<T> { T apply(T value); }
             | public abstract static class Body8 implements Service<String> {
             |   public String apply(String value) { return value; }
             | }
             | public abstract static class Body17 implements Service<String> {
             |   public String apply(String value) { return value; }
             | }
             | public abstract static class Body21 implements Service<String> {
             |   public String apply(String value) { return value; }
             | }
             | public static final class First extends Body$release {}
             | $future
             |}
             |""".stripMargin,
        )
        assertEquals(
          ToolProvider.getSystemJavaCompiler.run(
            null,
            null,
            null,
            "--release",
            release.toString,
            "-proc:none",
            "-d",
            classes.toString,
            source.toString,
          ),
          0,
        )
        val files = Files.walk(classes)
        try {
          for (
            file <- files.toArray
              .map(_.asInstanceOf[Path])
              .filter(Files.isRegularFile(_))
              .sortBy(_.toString)
          ) {
            val name = classes.relativize(file).toString.replace('\\', '/')
            val prefix =
              if (release == 8) "" else s"META-INF/versions/$release/"
            out.putNextEntry(new JarEntry(prefix + name))
            Files.copy(file, out)
            out.closeEntry()
          }
        } finally files.close()
      }
    } finally out.close()
    jar
  }

  private def request(
      declaration: String,
      expression: String,
      limit: Int = 8192,
      pc: JavaPresentationCompiler = presentationCompiler,
      copies: Int = 1,
  ): JsonObject = {
    val expressions = List.fill(copies)(expression).mkString("; ")
    val source =
      s"import fixture.dep.Api; class Demo { void run($declaration) { $expressions; } }"
    val uri = URI.create("file:///Demo.java")
    val hash = "sha256:" + MessageDigest
      .getInstance("SHA-256")
      .digest(source.getBytes(UTF_8))
      .map(b => f"${b & 0xff}%02x")
      .mkString
    val start = source.indexOf(expression)
    val targets = (0 until copies).map { index =>
      val offset = start + index * (expression.length + 2)
      JavaReceiverTarget(uri, hash, offset, offset + expression.length)
    }.asJava
    val inputs =
      List[VirtualFileParams](CompilerVirtualFileParams(uri, source)).asJava
    val result = pc
      .javaDependencyBodiesExperimental(inputs, targets, limit)
      .get(30, TimeUnit.SECONDS)
    assertEquals(
      result.get("schemaVersion").getAsString,
      "metals-java-dependency-bodies.experimental.v1",
    )
    assertEquals(
      result,
      pc.javaDependencyBodiesExperimental(inputs, targets, limit)
        .get(30, TimeUnit.SECONDS),
    )
    result
  }

  private def row(result: JsonObject): JsonObject =
    result
      .getAsJsonArray("documents")
      .get(0)
      .getAsJsonObject
      .getAsJsonArray("receiverSites")
      .get(0)
      .getAsJsonObject

  private def discovery(result: JsonObject): JsonObject = {
    val resultRow = row(result)
    assertEquals(resultRow.get("status").getAsString, "TYPED")
    val discovered = resultRow.getAsJsonObject("dependencyBodies")
    assert(!discovered.get("populationClosed").getAsBoolean)
    discovered
  }

  private def owners(discovered: JsonObject): Set[String] =
    discovered
      .getAsJsonArray("bodies")
      .asScala
      .map(_.getAsJsonObject.get("owner").getAsString)
      .toSet

  test("binary-only-unannotated-bodies-with-exact-artifact-provenance") {
    val result = request("Api.Service<String> value", """value.apply("x")""")
    val found = discovery(result)
    assertEquals(
      owners(found),
      Set("fixture.dep.Api.First", "fixture.dep.Api.Second"),
    )
    assertEquals(
      row(result).getAsJsonObject("selectedCallable").get("owner").getAsString,
      "fixture.dep.Api.Service",
    )
    val sha = "sha256:" + MessageDigest
      .getInstance("SHA-256")
      .digest(Files.readAllBytes(library))
      .map(b => f"${b & 0xff}%02x")
      .mkString
    for (
      body <- found.getAsJsonArray("bodies").asScala.map(_.getAsJsonObject)
    ) {
      assertEquals(body.get("artifactSha256").getAsString, sha)
      assertEquals(body.get("artifactUri").getAsString, library.toUri.toString)
      assertEquals(
        body.get("descriptor").getAsString,
        "(Ljava/lang/String;)Ljava/lang/String;",
      )
      assertEquals(body.get("proof").getAsString, "JAVAC_SUBTYPE_AND_OVERRIDE")
      assert(!body.get("abstract").getAsBoolean)
    }
    assert(
      found
        .getAsJsonArray("omissions")
        .asScala
        .exists(_.getAsString == "GENERIC_CANDIDATE_UNSUPPORTED")
    )
  }

  test("subclass-receiver-excludes-unrelated-contract-implementors") {
    assertEquals(
      owners(
        discovery(request("Api.Base<String> value", """value.apply("x")"""))
      ),
      Set("fixture.dep.Api.First"),
    )
  }

  for (release <- List(8, 17, 21)) {
    test(s"multi-release-uses-compiler-target-$release-not-runtime") {
      val pc = presentationCompiler
        .newInstance(
          s"mr-$release",
          List(multiReleaseLibrary).asJava,
          List("--release", release.toString, "-proc:none").asJava,
          () => Nil.asJava,
        )
        .asInstanceOf[JavaPresentationCompiler]
      try {
        val found = discovery(
          request("Api.Service<String> value", """value.apply("x")""", pc = pc)
        )
        val future =
          if (release == 21) Set("fixture.dep.Api.FutureOnly")
          else Set.empty[String]
        assertEquals(
          owners(found),
          Set(s"fixture.dep.Api.Body$release") ++ future,
        )
        assertEquals(
          found.get("selectedArtifactUri").getAsString,
          multiReleaseLibrary.toUri.toString,
        )
        assert(!found.getAsJsonArray("omissions").asScala.exists { reason =>
          Set("MULTI_RELEASE_UNSUPPORTED", "DUPLICATE_BINARY_NAME")
            .contains(reason.getAsString)
        })
        val bodies =
          found.getAsJsonArray("bodies").asScala.map(_.getAsJsonObject)
        assertEquals(
          bodies.map(_.get("receiverWitness").getAsString).toSet,
          Set("fixture.dep.Api.First") ++ future,
        )
        assert(
          bodies.forall(
            _.get("descriptor").getAsString ==
              "(Ljava/lang/String;)Ljava/lang/String;"
          )
        )
      } finally pc.shutdown()
    }
  }

  test("instantiated-generic-receiver-excludes-string-implementors") {
    assertEquals(
      owners(
        discovery(request("Api.Service<Integer> value", "value.apply(1)"))
      ),
      Set("fixture.dep.Api.NumberOnly"),
    )
  }

  test("overload-selection-is-not-method-name-matching") {
    val found = discovery(request("Api.Overloaded value", "value.apply(1)"))
    assertEquals(owners(found), Set("fixture.dep.Api.Overloads"))
    assertEquals(
      found
        .getAsJsonArray("bodies")
        .get(0)
        .getAsJsonObject
        .get("descriptor")
        .getAsString,
      "(I)Ljava/lang/String;",
    )
  }

  test("missing-implementation-does-not-close-empty-population") {
    assertEquals(
      owners(discovery(request("Api.Missing value", "value.missing()"))),
      Set.empty[String],
    )
  }

  test("raw-wildcard-and-concrete-selection-remain-outside-profile") {
    val raw = discovery(request("Api.Service value", """value.apply("x")"""))
    assertEquals(
      raw.get("status").getAsString,
      "UNSUPPORTED_SELECTION_OR_RECEIVER",
    )
    assertEquals(owners(raw), Set.empty[String])
    val wildcard = row(request("Api.Service<?> value", "value.apply(null)"))
    assertEquals(wildcard.get("status").getAsString, "UNSUPPORTED_TYPE")
    assert(!wildcard.has("dependencyBodies"))
    val concrete =
      discovery(request("Api.Unrelated value", """value.apply("x")"""))
    assertEquals(owners(concrete), Set.empty[String])
  }

  test("inventory-budget-is-visible-and-cannot-close") {
    val found = discovery(
      request("Api.Service<String> value", """value.apply("x")""", limit = 1)
    )
    assert(
      found
        .getAsJsonArray("omissions")
        .asScala
        .exists(_.getAsString == "INVENTORY_BUDGET")
    )
    assertEquals(owners(found), Set.empty[String])
    val invalid =
      request("Api.Service<String> value", """value.apply("x")""", limit = 0)
    assertEquals(invalid.get("status").getAsString, "INVALID_INVENTORY_BUDGET")
    val tooLarge =
      request(
        "Api.Service<String> value",
        """value.apply("x")""",
        limit = 262145,
      )
    assertEquals(tooLarge.get("status").getAsString, "INVALID_INVENTORY_BUDGET")
  }

  test("larger-inventory-is-opt-in-and-recovers-late-artifact") {
    val padding = Files.createTempFile("dependency-inventory-padding", ".jar")
    val out = new JarOutputStream(Files.newOutputStream(padding))
    try {
      for (i <- 0 until 65536) {
        out.putNextEntry(new JarEntry(s"resources/item-$i"))
        out.closeEntry()
      }
    } finally out.close()
    val pc = presentationCompiler
      .newInstance(
        "inventory",
        List(padding, library).asJava,
        javacOptions.asJava,
        () => Nil.asJava,
      )
      .asInstanceOf[JavaPresentationCompiler]
    try {
      val bounded = discovery(
        request(
          "Api.Service<String> value",
          """value.apply("x")""",
          limit = 65536,
          pc = pc,
        )
      )
      assertEquals(
        bounded.get("status").getAsString,
        "SELECTED_ARTIFACT_UNAVAILABLE",
      )
      assertEquals(bounded.get("inventoryEntriesSeen").getAsInt, 65536)
      val expanded = discovery(
        request(
          "Api.Service<String> value",
          """value.apply("x")""",
          limit = 262144,
          pc = pc,
        )
      )
      assertEquals(
        owners(expanded),
        Set("fixture.dep.Api.First", "fixture.dep.Api.Second"),
      )
      val work = expanded.getAsJsonObject("workMetrics")
      assertEquals(work.get("inventoryEntryLimit").getAsInt, 262144)
      assert(expanded.get("inventoryEntriesSeen").getAsInt > 65536)
      assertEquals(work.get("classpathEntriesPlanned").getAsInt, 2)
      assertEquals(work.get("classpathEntriesAttempted").getAsInt, 2)
      assertEquals(work.get("requestedReceiverCount").getAsInt, 1)
      assertEquals(
        work.get("receiverChecks").getAsInt,
        expanded.get("candidateTypeCount").getAsInt,
      )
      assert(work.get("membersIndexed").getAsInt > 0)
      assertEquals(
        work.get("membersSeen").getAsLong,
        work.get("membersIndexed").getAsLong,
      )
      assertEquals(work.get("bodyCount").getAsInt, 2)
      assert(
        !expanded
          .getAsJsonArray("omissions")
          .asScala
          .exists(_.getAsString == "INVENTORY_BUDGET")
      )
    } finally pc.shutdown()
  }

  test("production-batch-survives-former-receiver-check-budget") {
    // Actual javac subtype work: the 32-call batch must pass 100,000 checks
    // over unrelated candidates before the late First/Second witnesses.
    val unrelated = (0 until 3200)
      .map(i => s"public static final class Aaa$i {}")
      .mkString
    val artifact = buildLibrary(unrelated)
    val pc = presentationCompiler
      .newInstance(
        "receiver-budget",
        List(artifact).asJava,
        javacOptions.asJava,
        () => Nil.asJava,
      )
      .asInstanceOf[JavaPresentationCompiler]
    try {
      val result = request(
        "Api.Service<String> value",
        """value.apply("x")""",
        limit = 262144,
        pc = pc,
        copies = 32,
      )
      val rows = result
        .getAsJsonArray("documents")
        .get(0)
        .getAsJsonObject
        .getAsJsonArray("receiverSites")
        .asScala
      assertEquals(rows.size, 32)
      for (row <- rows) {
        val found = row.getAsJsonObject.getAsJsonObject("dependencyBodies")
        assertEquals(
          owners(found),
          Set("fixture.dep.Api.First", "fixture.dep.Api.Second"),
        )
        assert(!found.get("populationClosed").getAsBoolean)
        val work = found.getAsJsonObject("workMetrics")
        assertEquals(work.get("requestedReceiverCount").getAsInt, 32)
        assert(work.get("receiverChecks").getAsInt > 100000)
        assert(
          work.get("receiverChecks").getAsInt < work
            .get("receiverCheckLimit")
            .getAsInt
        )
        assert(
          !found
            .getAsJsonArray("omissions")
            .asScala
            .exists(
              _.getAsString.endsWith("_BUDGET")
            )
        )
      }
    } finally pc.shutdown()
  }

  test("unrelated-inherited-members-cannot-starve-receiver-catalog") {
    // Seventy unrelated witnesses each inherit 1,500 methods: the old
    // all-class catalog exhausted 100,000 members before First/Second.
    val methods = (0 until 1500).map(i => s"public void method$i() {}").mkString
    val subclasses = (0 until 70)
      .map(i => s"public static class Aaa$i extends AaaBase {}")
      .mkString
    val artifact = buildLibrary(
      s"public static class AaaBase { $methods } $subclasses"
    )
    val pc = presentationCompiler
      .newInstance(
        "relevant-members",
        List(artifact).asJava,
        javacOptions.asJava,
        () => Nil.asJava,
      )
      .asInstanceOf[JavaPresentationCompiler]
    try {
      val found = discovery(
        request("Api.Service<String> value", """value.apply("x")""", pc = pc)
      )
      assertEquals(
        owners(found),
        Set("fixture.dep.Api.First", "fixture.dep.Api.Second"),
      )
      assert(
        !found
          .getAsJsonArray("omissions")
          .asScala
          .exists(_.getAsString == "MEMBER_INVENTORY_BUDGET")
      )
    } finally pc.shutdown()
  }

  test("classpath-limit-admits-late-jar-but-remains-bounded") {
    val root = Files.createTempDirectory("dependency-path-budget")
    val directories =
      (0 until 4096).map(i => Files.createDirectory(root.resolve(s"empty-$i")))
    for (count <- List(256, 4096)) {
      val pc = presentationCompiler
        .newInstance(
          s"path-budget-$count",
          (directories.take(count) :+ library).asJava,
          javacOptions.asJava,
          () => Nil.asJava,
        )
        .asInstanceOf[JavaPresentationCompiler]
      try {
        val found = discovery(
          request("Api.Service<String> value", """value.apply("x")""", pc = pc)
        )
        if (count == 256) {
          assertEquals(
            owners(found),
            Set("fixture.dep.Api.First", "fixture.dep.Api.Second"),
          )
        } else {
          assertEquals(owners(found), Set.empty[String])
          assertEquals(
            found.get("status").getAsString,
            "SELECTED_ARTIFACT_UNAVAILABLE",
          )
          assert(
            found
              .getAsJsonArray("omissions")
              .asScala
              .exists(_.getAsString == "CLASSPATH_ENTRY_BUDGET")
          )
        }
      } finally pc.shutdown()
    }
  }

  test("duplicate-classpath-binary-identity-fails-closed") {
    val duplicate = library.resolveSibling("duplicate.jar")
    Files.copy(library, duplicate)
    val pc = presentationCompiler
      .newInstance(
        "duplicate",
        List(library, duplicate).asJava,
        javacOptions.asJava,
        () => Nil.asJava,
      )
      .asInstanceOf[JavaPresentationCompiler]
    try {
      val found = discovery(
        request("Api.Service<String> value", """value.apply("x")""", pc = pc)
      )
      assertEquals(
        found.get("status").getAsString,
        "SELECTED_ARTIFACT_UNAVAILABLE",
      )
      assertEquals(owners(found), Set.empty[String])
      assert(
        found
          .getAsJsonArray("omissions")
          .asScala
          .exists(_.getAsString == "DUPLICATE_BINARY_NAME")
      )
    } finally pc.shutdown()
  }

  test("binary-dollar-name-is-not-an-inherited-canonical-name") {
    val artifact = buildLibrary(
      """public interface Left { interface Name {} }
        |public interface Right { interface Name {} }
        |public static class AaaClash implements Left, Right {}
        |public static class AaaClash$Name {
        |  public static class Strict implements Service<String> {
        |    public String apply(String value) { return value; }
        |  }
        |}""".stripMargin
    )
    val pc = presentationCompiler
      .newInstance(
        "binary-dollar",
        List(artifact).asJava,
        javacOptions.asJava,
        () => Nil.asJava,
      )
      .asInstanceOf[JavaPresentationCompiler]
    try {
      val found = discovery(
        request("Api.Service<String> value", """value.apply("x")""", pc = pc)
      )
      assertEquals(
        owners(found),
        Set(
          "fixture.dep.Api.First",
          "fixture.dep.Api.Second",
          "fixture.dep.Api.AaaClash$Name.Strict",
        ),
      )
    } finally pc.shutdown()
  }

  test("cancelled-request-does-not-return-body-evidence") {
    val source =
      """import fixture.dep.Api; class Demo { void run(Api.Service<String> value) { value.apply("x"); } }"""
    val expression = """value.apply("x")"""
    val uri = URI.create("file:///CancelDemo.java")
    val hash = "sha256:" + MessageDigest
      .getInstance("SHA-256")
      .digest(source.getBytes(UTF_8))
      .map(b => f"${b & 0xff}%02x")
      .mkString
    val start = source.indexOf(expression)
    val polls = new java.util.concurrent.atomic.AtomicInteger()
    val token = new CancelToken {
      override def checkCanceled(): Unit =
        if (polls.incrementAndGet() >= 12) throw new CancellationException()
      override def onCancel(): CompletionStage[java.lang.Boolean] =
        CompletableFuture.completedFuture(java.lang.Boolean.FALSE)
    }
    val inputs = List[VirtualFileParams](
      CompilerVirtualFileParams(uri, source, token)
    ).asJava
    try {
      val result = presentationCompiler
        .javaDependencyBodiesExperimental(
          inputs,
          List(
            JavaReceiverTarget(uri, hash, start, start + expression.length)
          ).asJava,
        )
        .get(10, TimeUnit.SECONDS)
      assert(!result.toString.contains("JAVAC_SUBTYPE_AND_OVERRIDE"))
    } catch {
      case _: CancellationException => ()
      case error: ExecutionException =>
        assert(error.getCause.isInstanceOf[CancellationException])
    }
    assert(polls.get() >= 12)
  }
}
