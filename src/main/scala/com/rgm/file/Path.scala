package com.rgm.file

import java.io.{File => JFile, IOException}
import java.net.{URI, URL}
import java.nio.file.{Path => JPath, FileSystem => JFileSystem, _}
import java.nio.file.AccessMode._
import scala.language.implicitConversions
import scala.collection.JavaConverters._
import java.nio.file.attribute._
import java.util
import scala.util


object Path {
  def apply(jpath: JPath): Path = new Path(jpath)
  def apply(jfile: JFile): Path = new Path(jfile.toPath)

  implicit val defaultFileSys: JFileSystem = FileSystems.getDefault
  // it might be better if these methods took an implicit FileSystem rather than relying on java.nio.file..Paths
  // (which assumes the default FileSystem)
  def apply(uri: URI): Path = new Path(Paths.get(uri))
  def apply(path: String)(implicit fileSys: JFileSystem = defaultFileSys): Path = new Path(fileSys.getPath(path))

//  implicit def fromJPath(jpath: JPath): Path = apply(jpath)
//  implicit def fromJFile(jfile: JFile): Path = apply(jfile)
//  implicit def fromString(path: String): Path = apply(path)

//  implicit def toFinder(path: Path): PathFinder = ???


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

  def fileSystem: FileSystem = FileSystem(jpath.getFileSystem)

  def path: String = toString

  def name: String = if (path == fileSystem.separator) path else jpath.getFileName.toString

  //last most segment without extension
  def simpleName: String =
    if(extension == None)
      name
    else {
      name.dropRight(name.size - name.lastIndexWhere(_ == '.'))
    }

  def extension: Option[String] =
    if(name == ".." || path == "" || name.tail.count(_ == '.') == 0)
      None
    else
      Some(name.drop(name.lastIndexWhere(_ == '.') + 1))

  def withExtension(extension: Option[String]): Path =
    if (extension != None)
      sibling(simpleName + "." + extension)
    else
      this

  // segments should probably include the root segment, if any (like scalax but unlike Java NIO)
  def segments: Seq[Path] = segmentIterator.toSeq

  // just like segments
  def segmentIterator: Iterator[Path] =
    if (isAbsolute)
      Iterator(fileSystem.path(fileSystem.separator)) ++ jpath.iterator().asScala.map(Path(_))
    else
      jpath.iterator().asScala.map(Path(_))

  // again, should count the root segment, if any
  def segmentCount: Int = if (isAbsolute) jpath.getNameCount + 1 else jpath.getNameCount

  def root: Option[Path] = if (jpath.getRoot == null) None else Some(Path(jpath.getRoot))

  // there is a good argument for having this method always return an unwrapped (non-null) Path. For example:
  //   Path("a").parent == Path("") or Path(".")
  //   Path("").parent == Path("..")
  //   Path("..").parent == Path("../..")
  //   Path(fileSystem.separator).parent == Path(fileSystem.separator)
  // is this sensible, or does it present serious problems? it would certainly be convenient.
  // this breaks on ".." vs "../.." and "." and becomes inconsistent...

  def parent: Option[Path] = if (jpath.getParent == null) Option(null) else Option(Path(jpath.getParent))

  def subpath(begin: Int, end: Int): Path = Path(jpath.subpath(begin, end))

  def startsWith(other: Path): Boolean = jpath.startsWith(other.jpath)

  def startsWith(other: String): Boolean = jpath.startsWith(fileSystem.path(other).jpath)

  def endsWith(other: Path): Boolean = jpath.endsWith(other.jpath)

  def endsWith(other: String): Boolean = jpath.endsWith(fileSystem.path(other).jpath)

  def isAbsolute: Boolean = jpath.isAbsolute

  //this can't be syntactic function, needs to search filesystem for absolute path
  def toAbsolute: Path = Path(jpath.toAbsolutePath)

  // should behave mostly like JPath.normalize but should correctly handle the empty path
  // (N.B. the scalax implementation is wrong too--the operation should be idempotent).
  def normalize: Path =
    if (jpath.toString.equals(""))
      fileSystem.path("")
    else
      Path(jpath.normalize)

  def toURI: URI = jpath.toUri

  def toURL: URL = toURI.toURL

  def jfile: JFile = jpath.toFile

  // should behave like JPath.relativize (the scalax implementation is backwards and broken)
  // throws an error if you relativize relative and absolute paths
  def relativize(other: Path): Path = Path(jpath.relativize(other.jpath))

  def relativize(other: String): Path = Path(jpath.relativize(fileSystem.path(other).jpath))

  def relativeTo(base: Path): Path = base.relativize(this)

  def relativeTo(base: String): Path = fileSystem.path(base).relativize(this)

  // should behave like JPath.resolve except when `other` is an absolute path, in which case in should behave as if
  // `other` were actually a relative path (i.e. `other.relativeTo(other.root.get)`)
  // this is so that `path1 / path2` behaves exactly like `Path(path1.path + fileSystem.separator + path2.path)`
  def resolve(other: Path): Path =
  {
    if (other.isAbsolute)
      Path(jpath.resolve(other.relativeTo(other.root.get).jpath))
    else
      Path(jpath.resolve(other.jpath))
  }

  // as above
  def resolve(other: String): Path = resolve(fileSystem.path(other))

  def / (other: Path): Path = resolve(other)

  def / (other: String): Path = resolve(other)

  // should behave like JPath.resolveSibling except that `other` should always be treated as a relative path
  // (for consistency with resolve). the NIO implementation also seems dubious when `this` is the root or when `this`
  // has no parent and `other` is on a different file system
  def sibling(other: Path): Path = {
    if (this == root)
      throw new IOException("Root has no sibling")
    else if (parent == None)
      fileSystem.path("").resolve(other)
    else
      parent.get.resolve(other)

//    val sibl : Path = if (other.isAbsolute) Path(other.path.substring(1)) else other
//    if (this == (Path(fileSystem.separator)))
//      Path(fileSystem.path("/.").jpath.resolveSibling(sibl.path))
//    else
//      Path(jpath.resolveSibling(sibl.path))
  }

  def sibling(other: String): Path = sibling(fileSystem.path(other))

  //--------------------------------------------------------------------------------------------------------------------

  def toRealPath(options: LinkOption*): Path = Path(jpath.toRealPath(options : _*))

  def exists(): Boolean = Files.exists(jpath)

  def exists(options: LinkOption*): Boolean = Files.exists(jpath.toRealPath(options :_*))

  def nonExistent(): Boolean = Files.notExists(jpath) 

  def nonExistent(options: LinkOption*): Boolean = Files.notExists(jpath.toRealPath(options : _*))

  def isSame(other: Path): Boolean = normalize == other.normalize

  def size(): Option[Long] = Option(Files.size(jpath))

  def isDirectory(): Boolean = Files.isDirectory(jpath)

  def isFile(): Boolean = Files.isRegularFile(jpath)

  def isSymLink(): Boolean = Files.isSymbolicLink(jpath)

  def isHidden(): Boolean = Files.isHidden(jpath)

  def isReadable(): Boolean = Files.isReadable(jpath)

  def isWritable(): Boolean = Files.isWritable(jpath)

  def isExecutable(): Boolean = Files.isExecutable(jpath)

  // etc.

  //createTempFile
  def createTempFile(prefix: String, suffix: String, attrs: FileAttribute[_]*) : Path = Path(Files.createTempFile(jpath,prefix, suffix, attrs:_*))

  //createTempDir
  def createTempDir(prefix: String, attrs: FileAttribute[_]*) : Path = Path(Files.createTempDirectory(jpath, prefix, attrs:_*))

  //checkAccess -> canWrite, canRead, canExecute
  def checkAccess(modes: AccessMode*): Boolean = {
    modes forall {
      case EXECUTE  => isExecutable()
      case READ  => isReadable()
      case WRITE  => isWritable()
    }
  }

  //sets the access modes
  def setAccess(accessModes:Iterable[AccessMode]) = {
    jfile.setReadable(accessModes exists {_==READ})
    jfile.setWritable(accessModes exists {_==WRITE})
    jfile.setExecutable(accessModes exists {_==EXECUTE})
  }
  //lastModified
  def lastModified(): FileTime = Files.getLastModifiedTime(jpath)

  //sets POSIX file permissions
  def setFilePerm(perms: Set[PosixFilePermission]) : Path = Path(Files.setPosixFilePermissions(jpath, perms.asJava))

  //createFile
  def createFile(): Path = Path(Files.createFile(jpath))

  //createDirectory
  def createDirectory(): Path = Path(Files.createDirectory(jpath))

  //deleteIfExists
  def deleteIfExists(): Boolean = Files.deleteIfExists(jpath)

  //delete
  def delete() : Unit = Files.delete(jpath)

  //deleteRecursively
  def deleteRecursively(): Boolean =
  {
    //first check if it's a dir or file
    if(exists && isDirectory)
    {
      Files.walkFileTree(jpath,
        new SimpleFileVisitor[JPath]
        {
          //@throws(classOf[IOException])
          override def postVisitDirectory(dir: JPath, e: IOException) : FileVisitResult =
          {
            if(e != null)
              throw e
            else
            {
              Files.delete(dir)
              FileVisitResult.CONTINUE
            }
          }

          override def visitFile(file: JPath,attrs: BasicFileAttributes ) : FileVisitResult = {Files.delete(file);FileVisitResult.CONTINUE }
        }
      )
      true
    }
    else if(exists())
    {
      try {
        delete()
        true
      } catch {
        case e: IOException => false
      }
    }
    else
      false
  }

  //copyTo(source, target)
  def copyTo(target: Path, options: CopyOption*) : Path = Path(Files.copy(jpath, target.jpath, options:_*))


  //moveFile
  def moveFile(target: Path, options: CopyOption*) : Unit = Files.move(jpath, target.jpath, options:_*)

  //moveDirectory
  def moveDirectory(target: Path) : Unit =
  {
    if(exists && isDirectory)
    {
      Files.walkFileTree(jpath,
        new SimpleFileVisitor[JPath]
        {
          //@throws(classOf[IOException])
          override def preVisitDirectory(dir: JPath, attrs: BasicFileAttributes) : FileVisitResult = {
            Files.createDirectories(target.resolve(Path(jpath.relativize(dir))).jpath)
            FileVisitResult.CONTINUE
          }

          override def visitFile(file: JPath,attrs: BasicFileAttributes) : FileVisitResult = {
            Files.move(file, target.resolve(Path(jpath.relativize(file))).jpath)
            FileVisitResult.CONTINUE
          }

          override def postVisitDirectory(dir: JPath, e: IOException) : FileVisitResult = {
            Files.delete(dir)
            FileVisitResult.CONTINUE
          }
        })
    }
  }
}
