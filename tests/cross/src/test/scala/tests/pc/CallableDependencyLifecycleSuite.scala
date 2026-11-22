package tests.pc

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.TimeUnit

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.internal.semanticdb.Diagnostic
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.pc.SourcePathMode

import tests.BasePCSuite

/** Distinguish caller-body invalidation from replacement of its dependencies. */
class CallableDependencyLifecycleSuite extends BasePCSuite {
  protected def sourcePathMode: SourcePathMode = SourcePathMode.PRUNED

  override protected def config: PresentationCompilerConfigImpl =
    PresentationCompilerConfigImpl(sourcePathMode = sourcePathMode)

  override protected def sourceFiles: Seq[Path] =
    List("Owners.scala", "Caller.scala").map(tmp.resolve(_).toNIO)

  private val owner =
    """|package fixture
       |final case class Entity(value: String)
       |object Entity { def apply(value: Int): Entity = new Entity(value.toString) }
       |trait SettingsCompanion[T] {
       |  final def apply(value: String): T = from(value)
       |  def from(value: String): T
       |}
       |object Settings extends SettingsCompanion[Entity] {
       |  def from(value: String): Entity = Entity(value)
       |}
       |""".stripMargin
  private val caller =
    """|package fixture
       |object Caller {
       |  def generated: Entity = Entity("generated")
       |  def authored: Entity = Entity(1)
       |  def inherited: Entity = Settings("settings")
       |  def anonymous: Runnable = new Runnable {
       |    def run(): Unit = {
       |      val user = Entity("anonymous")
       |      (1 to 10).foreach(_ => println(user.value))
       |      throw new RuntimeException("failure")
       |    }
       |  }
       |}
       |""".stripMargin

  for (transition <- List("full", "pruned", "outline"))
    test(
      s"caller-after-dependency-${sourcePathMode.toString.toLowerCase}-$transition"
    ) {
      presentationCompiler.restart()
      val root = tmp.toNIO
      val sources = List("Owners.scala" -> owner, "Caller.scala" -> caller)
      sources.foreach { case (name, text) =>
        Files.write(root.resolve(name), text.getBytes(UTF_8))
      }
      def input(name: String, text: String) = CompilerVirtualFileParams(
        root.resolve(name).toUri,
        text,
        shouldReturnDiagnostics = true
      )
      def document(params: CompilerVirtualFileParams): TextDocument =
        TextDocument.parseFrom(
          presentationCompiler
            .semanticdbTextDocument(params)
            .get(30, TimeUnit.SECONDS)
        )
      def clean(doc: TextDocument): Unit = {
        assertEquals(
          doc.diagnostics.filter(_.severity == Diagnostic.Severity.ERROR),
          Nil
        )
        assert(doc.occurrences.nonEmpty)
      }
      val before = document(input("Caller.scala", caller))
      clean(before)
      assert(before.synthetics.nonEmpty)
      for (_ <- 0 until 3) {
        val ownerInput = input("Owners.scala", owner)
        val transformed = transition match {
          case "pruned" => ownerInput.copy(shouldPruneSemanticdb = true)
          case "outline" =>
            ownerInput.copy(outlineFiles =
              java.util.Optional.of(
                new scala.meta.pc.OutlineFiles {
                  override def isFirstCompileSubstitute(): Boolean = false
                  override def files()
                      : java.util.List[scala.meta.pc.VirtualFileParams] =
                    List[scala.meta.pc.VirtualFileParams](ownerInput).asJava
                }
              )
            )
          case _ => ownerInput
        }
        document(transformed)
        clean(document(ownerInput))
        val after = document(input("Caller.scala", caller))
        clean(after)
        assertEquals(after.synthetics, before.synthetics)
        assertEquals(after.occurrences, before.occurrences)
      }
    }
}

class CallableDependencyLifecycleFullSuite
    extends CallableDependencyLifecycleSuite {
  override protected def sourcePathMode: SourcePathMode = SourcePathMode.FULL
}
