package com.rgm.file

import java.io.{File => JFile}
import java.net.{URI, URL}
import java.nio.file.{Path => JPath, Paths, Files, LinkOption}
import scala.language.implicitConversions

object Path {
  def apply(jpath: JPath): Path = new Path(jpath)
  def apply(jfile: JFile): Path = new Path(jfile.toPath)

  // it might be better if these methods took an implicit FileSystem rather than relying on java.nio.file..Paths
  // (which assumes the default FileSystem)
  def apply(uri: URI): Path = new Path(Paths.get(uri))
  def apply(path: String): Path = new Path(Paths.get(path))

  implicit def fromJPath(jpath: JPath): Path = apply(jpath)
  implicit def fromJFile(jfile: JFile): Path = apply(jfile)
  implicit def fromString(path: String): Path = apply(path)

  implicit def toFinder(path: Path): PathFinder = ???
}

final class Path(val jpath: JPath) extends Equals with Ordered[Path] {

  if (jpath == null) throw new NullPointerException("cannot wrap a null path")

  override def toString: String = jpath.toString

  override def hashCode: Int = jpath.hashCode

  override def equals(other: Any): Boolean = other match {
    case that: Path => this.jpath equals that.jpath
    case _ => false
  }

  def canEqual(that: Any): Boolean = that.isInstanceOf[Path]

  def compare(that: Path): Int = jpath.compareTo(that.jpath)

  //--------------------------------------------------------------------------------------------------------------------

  def fileSystem: FileSystem = ???

  def path: String = toString

  def name: String = ???

  def simpleName: String = ???

  def extension: Option[String] = ???

  def withExtension(extension: Option[String]): Path = ???

  // segments should probably include the root segment, if any (like scalax but unlike Java NIO)
  def segments: Seq[Path] = ???

  // just like segments
  def segmentIterator: Iterator[Path] = ???

  // again, should count the root segment, if any
  def segmentCount: Int = ???

  def root: Option[Path] = ???

  // there is a good argument for having this method always return an unwrapped (non-null) Path. For example:
  //   Path("a").parent == Path("") or Path(".")
  //   Path("").parent == Path("..")
  //   Path("..").parent == Path("../..")
  //   Path("/").parent == Path("/")
  // is this sensible, or does it present serious problems? it would certainly be convenient.
  def parent: Option[Path] = ???

  def subpath(begin: Int, end: Int): Path = ???

  def startsWith(other: Path): Boolean = ???

  def startsWith(other: String): Boolean = ???

  def endsWith(other: Path): Boolean = ???

  def endsWith(other: String): Boolean = ???

  def isAbsolute: Boolean = ???

  def toAbsolute: Path = ???

  // should behave mostly like JPath.normalize but should correctly handle the empty path
  // (N.B. the scalax implementation is wrong too--the operation should be idempotent).
  def normalize: Path = ???

  def toRealPath(options: LinkOption*): Path = ???

  def toURI: URI = ???

  def toURL: URL = ???

  def jfile: JFile = jpath.toFile

  // should behave like JPath.relativize (the scalax implementation is backwards and broken)
  def relativize(other: Path): Path = ???

  def relativize(other: String): Path = ???

  def relativeTo(base: Path): Path = base.relativize(this)

  def relativeTo(base: String): Path = ???

  // should behave like JPath.resolve except when `other` is an absolute path, in which case in should behave as if
  // `other` were actually a relative path (i.e. `other.relativeTo(other.root.get)`)
  // this is so that `path1 / path2` behaves exactly like `Path(path1.path + "/" + path2.path)`
  def resolve(other: Path): Path = ???

  // as above
  def resolve(other: String): Path = ???

  def / (other: Path): Path = resolve(other)

  def / (other: String): Path = resolve(other)

  // should behave like JPath.resolveSibling except that `other` should always be treated as a relative path
  // (for consistency with resolve). the NIO implementation also seems dubious when `this` is the root or when `this`
  // has no parent and `other` is on a different file system
  def sibling(other: Path): Path = ???

  def sibling(other: String): Path = ???

  //--------------------------------------------------------------------------------------------------------------------

  def exists: Boolean = Files.exists(jpath)

  def exists(options: LinkOption*): Boolean = ???

  def nonExistent: Boolean = Files.notExists(jpath) // not equivalent to `!exists`

  def nonExistent(options: LinkOption*): Boolean = ???

  def isSame(other: Path): Boolean = ???

  def size: Option[Long] = ???

  def isDirectory: Boolean = ???

  def isFile: Boolean = ??? // == isRegularFile

  def isSymLink: Boolean = ???

  def isHidden: Boolean = ???

  def isReadable: Boolean = ???

  def isWritable: Boolean = ???

  def isExecutable: Boolean = ???

  // etc.

}
