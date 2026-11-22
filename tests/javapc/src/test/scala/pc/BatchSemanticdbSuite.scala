package pc

import java.net.URI

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.jsemanticdb.Semanticdb
import scala.meta.internal.metals.CompilerVirtualFileParams

import tests.pc.BaseJavaSemanticdbSuite

class BatchSemanticdbSuite extends BaseJavaSemanticdbSuite {

  test("java-symbol-language") {
    val contract = """package fixture;
                     |public interface Contract<T> {
                     |  T read(T value);
                     |  interface Port { String value(); }
                     |}
                     |""".stripMargin
    val implementation =
      """package fixture;
        |public class Implementation implements Contract<String> {
        |  public String read(String value) { return value; }
        |  public String read(long value) { return Long.toString(value); }
        |}
        |""".stripMargin
    val documents = batchTextDocuments(
      List(
        CompilerVirtualFileParams(
          URI.create("file:///Contract.java"),
          contract,
        ),
        CompilerVirtualFileParams(
          URI.create("file:///Implementation.java"),
          implementation,
        ),
      )
    )
    assertEquals(documents.size, 2)
    documents.foreach { document =>
      assertEquals(document.getLanguage(), Semanticdb.Language.JAVA)
      assertEquals(document.getDiagnosticsCount(), 0)
      val symbols = document.getSymbolsList().asScala
      assert(symbols.nonEmpty)
      symbols.foreach { information =>
        assertEquals(
          information.getLanguage(),
          Semanticdb.Language.JAVA,
          information.getSymbol(),
        )
      }
    }
    val contractDocument =
      documents.find(_.getUri() == "file:///Contract.java").get
    val single = textDocument(contract, URI.create("file:///Contract.java"))
    assertEquals(single, contractDocument)
    val methods = documents
      .flatMap(_.getSymbolsList().asScala)
      .filter(
        _.getKind() == Semanticdb.SymbolInformation.Kind.METHOD
      )
    assertEquals(methods.count(_.getDisplayName() == "read"), 3)
    val overrides = methods.filter(_.getOverriddenSymbolsCount() > 0)
    assertEquals(overrides.size, 1)
    assertEquals(
      overrides.head.getOverriddenSymbolsList().asScala.toSeq,
      Seq("fixture/Contract#read()."),
    )
  }

  checkBatch(
    "basic",
    Map(
      "A.java" -> """package a;
                    |public class A {
                    |    public String foo() {
                    |        return b.B.greeting();
                    |    }
                    |}
      """.stripMargin,
      "B.java" -> """
                    |package b;
                    |public class B {
                    |    public static String greeting() {
                    |        return "Hello, world!";
                    |    }
                    |}
      """.stripMargin,
    ),
    """|==========
       |file:///A.java
       |   package a;
       |//         ^ reference a/
       |   public class A {
       |//              ^ definition a/A#
       |//              ^ definition a/A#`<init>`().
       |       public String foo() {
       |//            ^^^^^^ reference java/lang/String#
       |//                   ^^^ definition a/A#foo().
       |           return b.B.greeting();
       |//                ^ reference b/
       |//                  ^ reference b/B#
       |//                    ^^^^^^^^ reference b/B#greeting().
       |       }
       |   }
       |      
       |==========
       |file:///B.java
       |
       |   package b;
       |//         ^ reference b/
       |   public class B {
       |//              ^ definition b/B#
       |//              ^ definition b/B#`<init>`().
       |       public static String greeting() {
       |//                   ^^^^^^ reference java/lang/String#
       |//                          ^^^^^^^^ definition b/B#greeting().
       |           return "Hello, world!";
       |       }
       |   }
       |""".stripMargin,
  )
}
