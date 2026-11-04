package tests.pc

import java.net.URI
import java.util.concurrent.TimeUnit

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.pc.ScalaConstructorEvidence

import tests.BasePCSuite

class ConstructorEvidenceSuite extends BasePCSuite {
  override def munitIgnore: Boolean = !scalaVersion.startsWith("2.13.")

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

  test("anonymous-and-generic-signatures-abstain-without-partial-targets") {
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
      rows.exists(_.reason == "ANONYMOUS_CONSTRUCTOR"),
      clues(rows.map(_.reason))
    )
    assert(rows.forall(_.status == "UNAVAILABLE"), clues(rows.map(_.status)))
    assert(
      rows.forall(r =>
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
}
