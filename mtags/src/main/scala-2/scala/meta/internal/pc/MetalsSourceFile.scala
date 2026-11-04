package scala.meta.internal.pc

import java.net.URI
import java.nio.file.Paths

import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.AbstractFile
import scala.reflect.io.PlainFile
import scala.reflect.io.VirtualFile
import scala.util.Try

import scala.meta.pc.VirtualFileParams

/**
 * An implementation of BatchSourceFile that implements equality based on virtual file path name.
 *
 * It is unclear why the default implementation special-cases virtual files. The presentation compiler
 * uses source file equality to prune the `allSources` list of managed files (that are compiled by the
 * background compilation thread).
 */
class MetalsSourceFile(filename: String, contents: Array[Char])
    extends BatchSourceFile(MetalsSourceFile.file(filename), contents) {

  def this(params: VirtualFileParams) = {
    this(params.uri().toString, params.text().toCharArray)
  }

  override def equals(that: Any): Boolean = that match {
    case other: MetalsSourceFile => this.file == other.file
    case _ => super.equals(that)
  }

  override def hashCode(): Int = path.hashCode()
}

object MetalsSourceFile {
  // Source-path loaders use PlainFile. Using a VirtualFile for the same file URI
  // splits both compiler units and nominal symbols when that source is opened.
  // Only the identity comes from disk: BatchSourceFile still uses the caller's
  // contents, including unsaved edits and files which have not been saved yet.
  private[pc] def file(filename: String): AbstractFile = {
    val physicalFile = for {
      uri <- Try(new URI(filename)).toOption
      if uri.getScheme == "file"
      file <- Try(
        new PlainFile(scala.reflect.io.Path(Paths.get(uri).toFile))
      ).toOption
    } yield file
    physicalFile.getOrElse(new VirtualFile(filename))
  }
}
