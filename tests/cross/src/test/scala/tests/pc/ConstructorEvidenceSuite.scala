package tests.pc

import java.net.URI
import java.util.concurrent.TimeUnit

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.pc.ScalaConstructorEvidence

import tests.BasePCSuite

class ConstructorEvidenceSuite extends BasePCSuite {
  override def munitIgnore: Boolean = !scalaVersion.startsWith("2.")

  private def evidence(source: String): ScalaConstructorEvidence =
    presentationCompiler
      .scalaConstructorEvidence(
        CompilerVirtualFileParams(new URI("file:///Constructor.scala"), source)
      )
      .get(30, TimeUnit.SECONDS)

  test("selected-same-arity-overloads-alias-and-utf16") {
    val source =
      """package fixture
        |final class Choice(value: Int) {
        |  def this(value: String) = this(value.length)
        |}
        |object Caller {
        |  /* 😀 */ val integer = new Choice(1)
        |  val text = new Choice("x")
        |  val alias = new RuntimeException("fixture")
        |}
        |""".stripMargin
    val result = evidence(source)
    assertEquals(result.status, "AVAILABLE", clues(result.reason))
    assertEquals(result.schemaVersion, ScalaConstructorEvidence.SCHEMA_VERSION)
    assert(result.sourceMd5.nonEmpty)
    val rows = result.invocations.asScala.toSeq
    assertEquals(rows.size, 3)
    assert(rows.forall(_.status == "AVAILABLE"), clues(rows.map(_.reason)))
    assertEquals(rows(0).declaringOwnerSymbol, "fixture/Choice#")
    assertEquals(rows(1).declaringOwnerSymbol, "fixture/Choice#")
    assertNotEquals(rows(0).selectedSymbol, rows(1).selectedSymbol)
    assertEquals(rows(0).parameterLists.get(0).get(0).tpe.symbol, "scala/Int#")
    assertEquals(
      rows(1).parameterLists.get(0).get(0).tpe.symbol,
      "java/lang/String#"
    )
    assertEquals(rows(2).declaringOwnerFqn, "java.lang.RuntimeException")
    assertEquals(rows(2).declaringOwnerSymbol, "java/lang/RuntimeException#")
    assert(rows(2).selectedSymbol.contains("`<init>`"))
    val line = source.linesIterator.toSeq(5)
    assertEquals(rows(0).calleeRange.getStart.getLine, 5)
    assertEquals(
      rows(0).calleeRange.getStart.getCharacter,
      line.indexOf("Choice")
    )
    assertEquals(
      rows(0).calleeRange.getEnd.getCharacter,
      line.indexOf("Choice") + "Choice".length
    )
    assertEquals(
      rows(0).invocationRange.getStart.getCharacter,
      line.indexOf("new Choice")
    )
    assertEquals(rows(0).invocationRange.getEnd.getCharacter, line.length)
    val repeated = evidence(source)
    assertEquals(
      repeated.invocations.asScala.map(_.selectedSymbol).toSeq,
      rows.map(_.selectedSymbol)
    )
  }

  test("compiler-error-does-not-export-surviving-positive-tree") {
    val result = evidence(
      "package fixture; class Choice(value: Int); object Caller { val ok = new Choice(1); val bad = absent }"
    )
    assertEquals(result.status, "UNAVAILABLE")
    assertEquals(result.reason, "COMPILER_ERROR")
    assert(result.invocations.isEmpty)
  }

  test("anonymous-allocation-selects-its-own-initializer") {
    val source =
      """package fixture
        |trait Handler { def run(): Int }
        |object Caller {
        |  val value = new Handler { val initialized = 1; def run(): Int = initialized }
        |}
        |""".stripMargin
    val result = evidence(source)
    assertEquals(result.status, "AVAILABLE", clues(result.reason))
    val rows = result.invocations.asScala.toSeq
    assertEquals(rows.size, 1)
    assertEquals(rows.head.status, "AVAILABLE", clues(rows.head.reason))
    val own = rows.head.anonymousConstructor
    assert(own != null)
    assert(own.ownerSymbol.nonEmpty && own.constructorSymbol.nonEmpty)
    assertEquals(own.initializerKind, "ANONYMOUS_CLASS_INITIALIZATION")
    assertEquals(own.parameterSemantics, "TYPED_ALLOCATION_BEFORE_LAMBDA_LIFT")
    assert(own.parentSymbols.asScala.contains("fixture/Handler#"))
    assertEquals(rows.head.selectedSymbol, null)
    assertEquals(rows.head.declaringOwnerFqn, null)
    assertEquals(rows.head.constructedType, null)
    assert(rows.head.parameterLists.isEmpty)
    assertEquals(rows.head.invocationRange, own.ownerRange)
    assertEquals(own.ownerRange.getStart.getLine, 3)
    assertEquals(own.ownerRange.getStart.getCharacter, "  val value = ".length)
    assertEquals(
      own.ownerRange.getEnd.getCharacter,
      source.linesIterator.toSeq(3).length
    )
    val warm = evidence(source).invocations.get(0).anonymousConstructor
    presentationCompiler.restart()
    val fresh = evidence(source).invocations.get(0).anonymousConstructor
    for (other <- Seq(warm, fresh)) {
      assertEquals(other.ownerRange, own.ownerRange)
      assertEquals(other.parentSymbols, own.parentSymbols)
      assertEquals(other.typedParameterCount, own.typedParameterCount)
      assertEquals(other.typedArgumentCount, own.typedArgumentCount)
    }
  }

  test("anonymous-class-compound-generic-parent-captures-and-initializers") {
    val source =
      """package fixture
        |class Parent[A](val value: A) {
        |  def this(value: A, ignored: Int) = this(value)
        |}
        |trait Marker
        |class Outer(val field: Int) {
        |  def make(captured: Int) = new Parent[Int](captured + field, 2) with Marker {
        |    val initialized = captured + field
        |  }
        |}
        |object Other { trait Marker }
        |object Caller { val other = new Other.Marker {} }
        |""".stripMargin
    val result = evidence(source)
    assertEquals(result.status, "AVAILABLE", clues(result.reason))
    val anonymous =
      result.invocations.asScala.filter(_.anonymousConstructor != null).toSeq
    assertEquals(
      anonymous.size,
      2,
      clues(result.invocations.asScala.map(r => (r.status, r.reason)))
    )
    assert(anonymous.forall(_.status == "AVAILABLE"))
    val parents =
      anonymous.map(_.anonymousConstructor.parentSymbols.asScala.toSet)
    assert(
      parents.exists(p =>
        p.contains("fixture/Parent#") && p.contains("fixture/Marker#")
      )
    )
    assert(parents.exists(_.contains("fixture/Other.Marker#")), clues(parents))
    assertNotEquals(anonymous(0).invocationRange, anonymous(1).invocationRange)
    // Source super-constructor arguments are not the anonymous allocation ABI.
    assertEquals(anonymous.head.anonymousConstructor.typedArgumentCount, 0)
  }

  test("generic-named-signatures-abstain-without-partial-targets") {
    val result = evidence(
      """package fixture
        |class Box[A](value: A)
        |trait Handler { def run(): Unit }
        |object Caller {
        |  val generic = new Box[Int](1)
        |  val anonymous = new Handler { def run(): Unit = () }
        |}
        |""".stripMargin
    )
    assertEquals(result.status, "AVAILABLE", clues(result.reason))
    val rows = result.invocations.asScala.toSeq
    assert(
      rows.exists(_.reason == "UNSUPPORTED_SIGNATURE"),
      clues(rows.map(_.reason))
    )
    assert(
      rows.exists(_.anonymousConstructor != null),
      clues(rows.map(_.reason))
    )
    val rejected = rows.filter(_.status == "UNAVAILABLE")
    assertEquals(rejected.size, 1)
    assert(
      rejected.forall(r =>
        r.selectedSymbol == null && r.constructedType == null && r.parameterLists.isEmpty
      )
    )
  }

  test("repeated-by-name-and-path-dependent-formals-abstain") {
    val result = evidence(
      """package fixture
        |class Repeated(values: Int*)
        |class Deferred(value: => Int)
        |class Outer { class Inner(value: Int) }
        |object Caller {
        |  val repeated = new Repeated(1, 2)
        |  val deferred = new Deferred(1)
        |  val outer = new Outer()
        |  val inner = new outer.Inner(1)
        |}
        |""".stripMargin
    )
    assertEquals(result.status, "AVAILABLE", clues(result.reason))
    val rows = result.invocations.asScala.toSeq
    assertEquals(rows.count(_.status == "AVAILABLE"), 1)
    assertEquals(
      rows.find(_.status == "AVAILABLE").get.declaringOwnerSymbol,
      "fixture/Outer#"
    )
    val rejected = rows.filter(_.status == "UNAVAILABLE")
    assertEquals(rejected.size, 3)
    assert(rejected.forall(_.reason == "UNSUPPORTED_SIGNATURE"))
    assert(
      rejected.forall(r =>
        r.parameterLists.isEmpty && r.constructedType == null && r.selectedSymbol == null
      )
    )
  }

  test("source-bound-fails-before-compilation") {
    val result = evidence(" " * (4 * 1024 * 1024 + 1))
    assertEquals(result.status, "UNAVAILABLE")
    assertEquals(result.reason, "SOURCE_BOUND")
    assert(result.invocations.isEmpty)
  }

  test("source-path-loaded-unit-promotes-in-warm-compiler") {
    val source =
      """package fixture
        |final class Choice(value: Int)
        |object Caller {
        |  def build(): Choice = new Choice(1)
        |}
        |""".stripMargin
    val path = tmp.resolve("Promoted.scala").toNIO
    java.nio.file.Files.write(
      path,
      source.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    )
    try {
      val spc =
        presentationCompiler
          .asInstanceOf[scala.meta.internal.pc.ScalaPresentationCompiler]
      val uri = path.toUri
      val params = CompilerVirtualFileParams(uri, source)
      spc.compilerAccess
        .withNonInterruptableCompiler(
          (),
          scala.meta.internal.metals.EmptyCancelToken
        ) { pc =>
          val compiler = pc.compiler()
          val sf = new scala.meta.internal.pc.MetalsSourceFile(
            uri.toString,
            source.toCharArray
          ).file
          compiler.PruneLateSourcesComponent.loadedFromSource.add(sf)
        }(spc.emptyQueryContext)
        .get(30, TimeUnit.SECONDS)

      val result = presentationCompiler
        .scalaConstructorEvidence(params)
        .get(30, TimeUnit.SECONDS)
      assertEquals(result.status, "AVAILABLE", clues(result.reason))
      assertEquals(result.invocations.size(), 1)
      assertEquals(
        result.invocations.get(0).declaringOwnerSymbol,
        "fixture/Choice#"
      )

      val promoted = spc.compilerAccess
        .withNonInterruptableCompiler(
          false,
          scala.meta.internal.metals.EmptyCancelToken
        ) { pc =>
          val compiler = pc.compiler()
          val sf = new scala.meta.internal.pc.MetalsSourceFile(
            uri.toString,
            source.toCharArray
          ).file
          !compiler.PruneLateSourcesComponent.loadedFromSource.contains(sf) &&
          compiler.unitOfFile.get(sf).exists(_.isTypeChecked)
        }(spc.emptyQueryContext)
        .get(30, TimeUnit.SECONDS)
      assert(promoted, "warm compiler must retain the promoted unpruned unit")
    } finally {
      java.nio.file.Files.deleteIfExists(path)
    }
  }
}
