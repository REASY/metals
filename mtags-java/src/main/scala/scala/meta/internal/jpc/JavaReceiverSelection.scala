package scala.meta.internal.jpc

import javax.lang.model.`type`.ArrayType
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.ExecutableType
import javax.lang.model.`type`.TypeKind
import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.ExecutableElement
import javax.lang.model.element.Modifier
import javax.lang.model.element.TypeElement
import javax.lang.model.util.Elements

import scala.jdk.CollectionConverters._

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import com.sun.source.util.JavacTask
import com.sun.source.util.TreePath
import com.sun.source.util.Trees
import com.sun.tools.javac.code.Symbol

/** Declaration identity is separate from the expression's instantiated type. */
private[jpc] object JavaReceiverSelection {

  /** Javac's selected declaring classfile, never a source-layout guess. */
  def origin(task: JavacTask, path: TreePath): JsonObject = {
    val row = new JsonObject()
    Trees.instance(task).getElement(path) match {
      case method: ExecutableElement =>
        method.getEnclosingElement match {
          case owner: Symbol.ClassSymbol
              if Trees.instance(task).getPath(owner) == null &&
                owner.classfile != null =>
            val uri = owner.classfile.toUri
            if (uri.toString.length > 8192)
              row.addProperty("status", "ORIGIN_URI_BUDGET")
            else if (uri.getScheme != "file")
              row.addProperty("status", "NON_DIRECTORY_CLASSFILE")
            else {
              row.addProperty("status", "CLASSFILE_ORIGIN")
              row.addProperty("uri", uri.toString)
              val modifiers = new JsonArray()
              method.getModifiers.asScala
                .map(_.name())
                .toSeq
                .sorted
                .foreach(
                  modifiers.add
                )
              row.add("methodModifiers", modifiers)
            }
          case _ => row.addProperty("status", "SOURCE_ORIGIN_OR_UNAVAILABLE")
        }
      case _ => row.addProperty("status", "SELECTION_UNAVAILABLE")
    }
    row
  }

  def selected(task: JavacTask, path: TreePath): Option[JsonObject] = {
    Trees.instance(task).getElement(path) match {
      case method: ExecutableElement => callable(task, method)
      case _ => None
    }
  }

  private[jpc] def callable(
      task: JavacTask,
      method: ExecutableElement
  ): Option[JsonObject] = {
    val elements = task.getElements
    method.getEnclosingElement match {
      case owner: TypeElement =>
        task.getTypes.erasure(method.asType()) match {
          case erased: ExecutableType =>
            val parameters = erased.getParameterTypes.asScala.map(t =>
              descriptor(t, elements, 0)
            )
            val result = descriptor(erased.getReturnType, elements, 0)
            if (parameters.forall(_.isDefined) && result.isDefined) {
              val row = new JsonObject()
              row.addProperty("owner", owner.getQualifiedName.toString)
              row.addProperty(
                "binaryOwner",
                elements.getBinaryName(owner).toString
              )
              row.addProperty("member", method.getSimpleName.toString)
              row.addProperty(
                "descriptor",
                parameters.flatten.mkString("(", "", ")") + result
                  .getOrElse("")
              )
              row.addProperty(
                "abstract",
                method.getModifiers.contains(Modifier.ABSTRACT)
              )
              Some(row)
            } else None
          case _ => None
        }
      case _ => None
    }
  }

  private def descriptor(
      tpe: TypeMirror,
      elements: Elements,
      depth: Int
  ): Option[String] =
    if (depth > 8) None
    else
      tpe.getKind match {
        case TypeKind.BOOLEAN => Some("Z")
        case TypeKind.BYTE => Some("B")
        case TypeKind.SHORT => Some("S")
        case TypeKind.INT => Some("I")
        case TypeKind.LONG => Some("J")
        case TypeKind.CHAR => Some("C")
        case TypeKind.FLOAT => Some("F")
        case TypeKind.DOUBLE => Some("D")
        case TypeKind.VOID => Some("V")
        case TypeKind.ARRAY =>
          tpe match {
            case array: ArrayType =>
              descriptor(array.getComponentType, elements, depth + 1).map(
                "[" + _
              )
            case _ => None
          }
        case TypeKind.DECLARED =>
          tpe match {
            case declared: DeclaredType =>
              declared.asElement() match {
                case owner: TypeElement =>
                  Some(
                    "L" + elements
                      .getBinaryName(owner)
                      .toString
                      .replace('.', '/') + ";"
                  )
                case _ => None
              }
            case _ => None
          }
        case _ => None
      }
}
