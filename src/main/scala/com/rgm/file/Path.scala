package com.rgm.file

import java.io.{File => JFile, _}
import java.net.{URI, URL}
import java.nio.file.{Path => JPath, FileSystem => JFileSystem, _}
import java.nio.file.AccessMode._
import scala.language.implicitConversions
import scala.collection.JavaConverters._
import java.nio.file.attribute._

import scala.Some


object Path {
  def apply(jpath: JPath): Path = new Path(jpath)
  def apply(jfile: JFile): Path = new Path(jfile.toPath)

  implicit val defaultFileSys: FileSystem = FileSystem(FileSystems.getDefault)

  def apply(uri: URI): Path = new Path(Paths.get(uri))
  def apply(path: String)(implicit fileSys: FileSystem = defaultFileSys): Path = fileSys.path(path)

  implicit def fromJPath(jpath: JPath): Path = apply(jpath)
  implicit def fromJFile(jfile: JFile): Path = apply(jfile)
  implicit def fromString(path: String): Path = apply(path)

  implicit def toSpec(path: Path): PathSpec = PathSpec(path)

  /** Creates temp file*/
  def createTempFile(dir: Path, prefix: String, suffix: String, attrs: FileAttribute[_]*) : Path = Path(Files.createTempFile(dir.jpath,prefix, suffix, attrs:_*))

  /**Creates temp directory*/
  def createTempDir(dir: Path, prefix: String, attrs: FileAttribute[_]*) : Path = Path(Files.createTempDirectory(dir.jpath, prefix, attrs:_*))



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

  def inputStream: InputStream = new FileInputStream(path)

  def outputStream: OutputStream = new FileOutputStream(path)

  def fileSystem: FileSystem = FileSystem(jpath.getFileSystem)

  /**Complete path*/
  def path: String = toString

  /**Last segment of path*/
  def name: String = if (path == fileSystem.separator) path else jpath.getFileName.toString

  /**Last segment, extension removed*/
  def simpleName: String =
    if(extension == None)
      name
    else {
      name.dropRight(name.size - name.lastIndexWhere(_ == '.'))
    }

  /**Extension, ignores leading dot on hidden files*/
  def extension: Option[String] =
    if(name == ".." || path == "" || name.tail.count(_ == '.') == 0)
      None
    else
      Some(name.drop(name.lastIndexWhere(_ == '.') + 1))

  /**Returns sibling with extension, replacing existing one if necessary*/
  def withExtension(extension: Option[String]): Path =
    if (extension != None)
      sibling(simpleName + "." + extension)
    else
      this

  /**Returns segments, including root*/
  def segments: Seq[Path] = segmentIterator.toSeq

  /**Just like segments */
  def segmentIterator: Iterator[Path] =
    if (isAbsolute)
      Iterator(fileSystem.path(fileSystem.separator)) ++ jpath.iterator().asScala.map(Path(_))
    else
      jpath.iterator().asScala.map(Path(_))

  def segmentCount: Int = if (isAbsolute) jpath.getNameCount + 1 else jpath.getNameCount

  def root: Option[Path] = if (jpath.getRoot == null) None else Some(Path(jpath.getRoot))

  // there is a good argument for having this method always return an unwrapped (non-null) Path. For example:
  //   Path("a").parent == Path("") or Path(".")
  //   Path("").parent == Path("..")
  //   Path("..").parent == Path("../..")
  //   Path(fileSystem.separator).parent == Path(fileSystem.separator)
  // is this sensible, or does it present serious problems? it would certainly be convenient.
  // this breaks on ".." vs "../.." and "." and becomes inconsistent...

  def parent: Option[Path] = if (jpath.getParent == null) None else Option(Path(jpath.getParent))

  /**Subpath starting at begin, stopping at end*/
  def subpath(begin: Int, end: Int): Path = Path(jpath.subpath(begin, end))

  /**Returns true if this starts with other*/
  def startsWith(other: Path): Boolean = jpath.startsWith(other.jpath)

  /**Returns true if this starts with other*/
  def startsWith(other: String): Boolean = jpath.startsWith(fileSystem.path(other).jpath)

  /**Returns true if this ends with other*/
  def endsWith(other: Path): Boolean = jpath.endsWith(other.jpath)

  /**Returns true if this ends with other*/
  def endsWith(other: String): Boolean = jpath.endsWith(fileSystem.path(other).jpath)

  def isAbsolute: Boolean = jpath.isAbsolute

  /**Touches disk*/
  def toAbsolute: Path = Path(jpath.toAbsolutePath)

  /**Returns path with redundancies removed.  Empty path maps to empty path*/
  def normalize: Path =
    if (jpath.toString.equals(""))
      fileSystem.path("")
    else
      Path(jpath.normalize)

  def toURI: URI = jpath.toUri

  def toURL: URL = toURI.toURL

  def jfile: JFile = jpath.toFile

  /**Relativizes from other to this*/
  def relativize(other: Path): Path = Path(jpath.relativize(other.jpath))

  def relativize(other: String): Path = Path(jpath.relativize(fileSystem.path(other).jpath))

  /**Relativize from this to base*/
  def relativeTo(base: Path): Path = base.relativize(this)

  def relativeTo(base: String): Path = fileSystem.path(base).relativize(this)

  /**Returns this concatenated with other.  If other is absolute, other is relativized first*/
  def resolve(other: Path): Path =
  {
    if (other.isAbsolute)
      Path(jpath.resolve(other.relativeTo(other.root.get).jpath))
    else
      Path(jpath.resolve(other.jpath))
  }

  def resolve(other: String): Path = resolve(fileSystem.path(other))

  def / (other: Path): Path = resolve(other)

  def / (other: String): Path = resolve(other)

  /**Returns other concatenated onto your parent
    *
    * If you have no parent other is concatenated with the empty path.  Root's
    * siblings are an error.*/
  def sibling(other: Path): Path = {
    if (this == root)
      throw new IOException("Root has no sibling")
    else if (parent == None)
      fileSystem.path("").resolve(other)
    else
      parent.get.resolve(other)

  }

  def sibling(other: String): Path = sibling(fileSystem.path(other))

  //--------------------------------------------------------------------------------------------------------------------

  def toRealPath(options: LinkOption*): Path = Path(jpath.toRealPath(options : _*))

  def exists(options: LinkOption*): Boolean = Files.exists(jpath,options:_*)

  def nonExistent(options: LinkOption*): Boolean = Files.notExists(jpath,options:_*)

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
  def deleteRecursively(): Boolean = {
    //first check if it's a dir or file
    if(exists() && isDirectory) {
      Files.walkFileTree(jpath,
        new SimpleFileVisitor[JPath] {
          //@throws(classOf[IOException])
          override def postVisitDirectory(dir: JPath, e: IOException) : FileVisitResult = {
            if(e != null)
              throw e
            else {
              Files.delete(dir)
              FileVisitResult.CONTINUE
            }
          }

          override def visitFile(file: JPath,attrs: BasicFileAttributes ) : FileVisitResult = {
            Files.delete(file);FileVisitResult.CONTINUE
          }
        }
      )
      true
    }
    else if(exists()) {
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
    if(exists() && isDirectory)
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
