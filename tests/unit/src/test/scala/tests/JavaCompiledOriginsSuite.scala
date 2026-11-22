package tests

import java.nio.file.Files

import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.metals.JavaCompiledClassfiles
import scala.meta.internal.metals.JavaCompiledOrigins

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes

class JavaCompiledOriginsSuite extends munit.FunSuite {
  private def classBytes(): Array[Byte] = {
    val writer = new ClassWriter(0)
    writer.visit(
      Opcodes.V17,
      Opcodes.ACC_PUBLIC,
      "fixture/Api",
      null,
      "java/lang/Object",
      null,
    )
    val method = writer.visitMethod(
      Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC,
      "value",
      "()I",
      null,
      null,
    )
    method.visitCode()
    method.visitInsn(Opcodes.ICONST_1)
    method.visitInsn(Opcodes.IRETURN)
    method.visitMaxs(1, 0)
    method.visitEnd()
    writer.visitEnd()
    writer.toByteArray
  }

  private def response(classUri: String): JsonObject = {
    val result = new JsonObject()
    result.addProperty("status", "DOCUMENT_RESULTS")
    val selected = new JsonObject()
    selected.addProperty("binaryOwner", "fixture.Api")
    selected.addProperty("member", "value")
    selected.addProperty("descriptor", "()I")
    selected.addProperty("abstract", false)
    val origin = new JsonObject()
    origin.addProperty("status", "CLASSFILE_ORIGIN")
    origin.addProperty("uri", classUri)
    val site = new JsonObject()
    site.add("selectedCallable", selected)
    site.add("selectedOrigin", origin)
    val sites = new JsonArray()
    sites.add(site)
    val document = new JsonObject()
    document.add("receiverSites", sites)
    val documents = new JsonArray()
    documents.add(document)
    result.add("documents", documents)
    result
  }

  test("selected-classfile-is-bounded-to-unique-output-and-exact-method") {
    val root = Files.createTempDirectory("java-compiled-origins")
    val pkg = Files.createDirectories(root.resolve("fixture"))
    val classfile = pkg.resolve("Api.class")
    Files.write(classfile, classBytes())
    try {
      val output =
        JavaCompiledOrigins.Output("file:///target", root.toUri.toString)
      val snapshot = JavaCompiledOrigins.Snapshot(
        Vector(output),
        "[[\"file:///target\",[],[\"" + root.toUri.toString + "\"]]]",
      )
      val selected = JavaCompiledOrigins.correlate(
        response(classfile.toUri.toString),
        Right(snapshot),
        Right(snapshot),
      )
      val enriched = JavaCompiledClassfiles.enrich(
        selected,
        Seq(root),
        EmptyCancelToken,
      )
      val origin = enriched
        .getAsJsonArray("documents")
        .get(0)
        .getAsJsonObject
        .getAsJsonArray("receiverSites")
        .get(0)
        .getAsJsonObject
        .getAsJsonObject("selectedOrigin")
      assertEquals(origin.get("outputStatus").getAsString, "BSP_OUTPUT_MEMBER")
      assertEquals(
        origin.get("classfileStatus").getAsString,
        "EXACT_DECLARATION",
      )
      assertEquals(
        origin
          .getAsJsonObject("classfileDeclaration")
          .get("bodyKind")
          .getAsString,
        "JVM_CODE",
      )
      assertEquals(enriched.getAsJsonArray("compiledClasses").size(), 1)
    } finally {
      Files.deleteIfExists(classfile)
      Files.deleteIfExists(pkg)
      Files.deleteIfExists(root)
    }
  }

  test("ambiguous-output-and-missing-declaration-stay-unavailable") {
    val root = Files.createTempDirectory("java-compiled-origins-negative")
    val nested = Files.createDirectories(root.resolve("nested"))
    val path = nested.resolve("Api.class")
    try {
      val parent = JavaCompiledOrigins.Output("parent", root.toUri.toString)
      val child = JavaCompiledOrigins.Output("child", nested.toUri.toString)
      assertEquals(
        JavaCompiledOrigins
          .outputOwner(path.toUri.toString, Vector(parent, child)),
        Left("AMBIGUOUS_OUTPUT_OWNER"),
      )
      val selected = new JsonObject()
      selected.addProperty("binaryOwner", "fixture.NotApi")
      selected.addProperty("member", "value")
      selected.addProperty("descriptor", "()I")
      selected.addProperty("abstract", false)
      assertEquals(
        JavaCompiledClassfiles
          .declaration(classBytes(), selected, EmptyCancelToken),
        Left("CLASSFILE_OWNER_MISMATCH"),
      )
    } finally {
      Files.deleteIfExists(nested)
      Files.deleteIfExists(root)
    }
  }

  test("changed-output-metadata-invalidates-entire-positive-response") {
    val first = JavaCompiledOrigins.Snapshot(Vector.empty, "[]")
    val second = JavaCompiledOrigins.Snapshot(Vector.empty, "[[\"changed\"]]")
    val result = JavaCompiledOrigins.correlate(
      response("file:///fixture/Api.class"),
      Right(first),
      Right(second),
    )
    assertEquals(
      result.get("status").getAsString,
      "BSP_OUTPUT_METADATA_CHANGED",
    )
    assertEquals(result.getAsJsonArray("documents").size(), 0)
  }
}
