package pc

import java.net.URI
import java.nio.charset.StandardCharsets

import scala.meta.internal.io.FileIO
import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.JdkSources
import scala.meta.internal.{semanticdb => s}

import coursierapi.Dependency
import tests.pc.BaseJavaSemanticdbSuite

class SemanticdbSuite extends BaseJavaSemanticdbSuite {

  test("overloaded-static-import-callsite") {
    val code =
      """|package overload;
         |import static org.assertj.core.api.Assertions.assertThat;
         |
         |class Caller {
         |  void check(String expected, String actual) {
         |    assertThat(actual).isEqualTo(expected);
         |  }
         |}
         |""".stripMargin
    val document = textDocument(code, URI.create("file:///Caller.java"))
    val callSymbols = document.getOccurrencesList.asScala
      .filter(occurrence =>
        occurrence.hasRange && occurrence.getRange.getStartLine == 5
      )
      .map(_.getSymbol)

    assert(
      callSymbols.exists(
        _.startsWith("org/assertj/core/api/Assertions#assertThat(")
      ),
      clues(callSymbols),
    )
    assert(
      callSymbols.exists(
        _.startsWith("org/assertj/core/api/AbstractStringAssert#isEqualTo(")
      ),
      clues(callSymbols),
    )
  }

  override protected def extraDependencies: Seq[Dependency] =
    Seq(Dependency.of("org.assertj", "assertj-core", "3.26.3"))

  check(
    "basic",
    """
      |class A {
      |    public int foo() {
      |        int x = 1;
      |        x = x + 1;
      |        return x;
      |    }
      |}
      """.stripMargin,
    """|   package basic;
       |//         ^^^^^ reference basic/
       |
       |   class A {
       |//       ^ definition basic/A#
       |//       ^ definition basic/A#`<init>`().
       |       public int foo() {
       |//                ^^^ definition basic/A#foo().
       |           int x = 1;
       |//             ^ definition local0
       |           x = x + 1;
       |//         ^ reference local0
       |//             ^ reference local0
       |           return x;
       |//                ^ reference local0
       |       }
       |   }
       |""".stripMargin,
  )

  check(
    "record",
    """
      |package example;
      |record Point(int x, int y) {
      |    public int sum() {
      |        return x + y;
      |    }
      |}
      """.stripMargin,
    """|package record;
       |//         ^^^^^^ reference record/
       |
       |   package example;
       |// ^ diagnostic - error class, interface, enum, or record expected
       |   record Point(int x, int y) {
       |//        ^^^^^ definition record/Point#
       |//        ^^^^^ definition record/Point#`<init>`().
       |//                  ^ definition local0
       |//                  ^ definition record/Point#x().
       |//                         ^ definition local1
       |//                         ^ definition record/Point#y().
       |       public int sum() {
       |//                ^^^ definition record/Point#sum().
       |           return x + y;
       |//                ^ reference record/Point#x().
       |//                    ^ reference record/Point#y().
       |       }
       |   }
       |""".stripMargin,
  )

  test("jdk") {
    val Right(jdkSources) = JdkSources()
    var isFound = false
    FileIO.withJarFileSystem(jdkSources, create = false, close = true) { root =>
      FileIO.listAllFilesRecursively(root).foreach { file =>
        if (file.toNIO.toString.endsWith("/java/lang/String.java")) {
          isFound = true
          val text = FileIO.slurp(file, StandardCharsets.UTF_8)
          val doc = textDocument(text, file.toNIO.toUri)
          assert(clue(doc).getOccurrencesCount() > 1_000)
        }
      }
    }
    assert(isFound, "java/lang/String.java not found")
  }

  test("scalameta-proto-compatibility") {
    val code =
      """|package protocompat;
         |
         |import java.lang.annotation.Retention;
         |import java.lang.annotation.RetentionPolicy;
         |
         |@Retention(value = RetentionPolicy.RUNTIME)
         |@interface MyAnnotation {
         |    String name() default "";
         |    int count() default 0;
         |}
         |
         |@MyAnnotation(name = "test", count = 42)
         |class AnnotatedClass {
         |    @MyAnnotation(name = "method")
         |    public void annotatedMethod() {}
         |}
         |""".stripMargin
    val uri = URI.create("file:///ProtoCompat.java")
    val pcParams = CompilerVirtualFileParams(uri, code)
    val bytes = presentationCompiler.semanticdbTextDocument(pcParams).get()

    val scalametaDoc = s.TextDocument.parseFrom(bytes)
    assert(
      scalametaDoc.uri.nonEmpty,
      "scalameta should parse jsemanticdb output",
    )

    val allAnnotations = scalametaDoc.symbols.flatMap(_.annotations)
    assertNoDiff(
      allAnnotations.mkString("\n"),
      """|AnnotationTree(TypeRef(Empty,java/lang/annotation/Retention#,Vector()),Vector(AssignTree(IdTree(java/lang/annotation/Retention#value().),SelectTree(IdTree(java/lang/annotation/RetentionPolicy#),Some(IdTree(java/lang/annotation/RetentionPolicy#RUNTIME.))))))
         |AnnotationTree(TypeRef(Empty,protocompat/MyAnnotation#,Vector()),Vector(AssignTree(IdTree(protocompat/MyAnnotation#name().),LiteralTree(StringConstant(test))), AssignTree(IdTree(protocompat/MyAnnotation#count().),LiteralTree(IntConstant(42)))))
         |AnnotationTree(TypeRef(Empty,protocompat/MyAnnotation#,Vector()),Vector(AssignTree(IdTree(protocompat/MyAnnotation#name().),LiteralTree(StringConstant(method)))))
         |""".stripMargin,
    )

    val assignTrees = allAnnotations.flatMap(_.arguments).collect {
      case s.AssignTree(lhs, rhs) => (lhs, rhs)
    }

    assertNoDiff(
      assignTrees.mkString("\n"),
      """|(IdTree(java/lang/annotation/Retention#value().),SelectTree(IdTree(java/lang/annotation/RetentionPolicy#),Some(IdTree(java/lang/annotation/RetentionPolicy#RUNTIME.))))
         |(IdTree(protocompat/MyAnnotation#name().),LiteralTree(StringConstant(test)))
         |(IdTree(protocompat/MyAnnotation#count().),LiteralTree(IntConstant(42)))
         |(IdTree(protocompat/MyAnnotation#name().),LiteralTree(StringConstant(method)))
         |""".stripMargin,
    )

  }

}
