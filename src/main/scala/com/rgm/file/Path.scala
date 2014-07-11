package com.rgm.file

import java.io.{File => JFile, OutputStream, InputStream, IOException}
import java.net.{URI, URL}
import java.nio.file.attribute._
import java.nio.file.{Path => JPath, _}
import scala.collection.JavaConverters._
import scala.language.implicitConversions
import scala.Some
import scala.util.{Failure, Success, Try}

object Path {
  def apply(jpath: JPath): Path = new Path(jpath)
  def apply(jfile: JFile): Path = new Path(jfile.toPath)

  def apply(uri: URI): Path = new Path(Paths.get(uri))
  def apply(path: String)(implicit fileSystem: FileSystem ): Path = fileSystem.path(path)

  implicit def fromJPath(jpath: JPath): Path = apply(jpath)
  implicit def fromJFile(jfile: JFile): Path = apply(jfile)
  implicit def fromString(path: String): Path = apply(path)

  implicit def toPathSpec(path: Path): PathSpec = PathSpec(path)

  /** Creates temp file*/
  def createTempFile(dir: Path = null, prefix: String = null, suffix: String = null,
                     attributes: TraversableOnce[FileAttribute[_]] = Nil): Path = {
    if (dir != null)
      Path(Files.createTempFile(dir.jpath, prefix, suffix, attributes.toArray: _*))
    else
      Path(Files.createTempFile(prefix, suffix, attributes.toArray: _*))
  }

  /**Creates temp directory*/
  def createTempDirectory(dir: Path = null, prefix: String = null,
                          attributes: TraversableOnce[FileAttribute[_]] = Nil): Path = {
    if (dir != null)
      Path(Files.createTempDirectory(dir.jpath, prefix, attributes.toArray: _*))
    else
      Path(Files.createTempDirectory(null, prefix, attributes.toArray: _*))
  }
}

final class Path(val jpath: JPath) extends Equals with Ordered[Path] {
  import java.nio.file.AccessMode._

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

  /** Returns a file system object using the underlying path */
  def fileSystem: FileSystem = FileSystem(jpath.getFileSystem)

  /** Complete path*/
  def path: String = jpath.toString

  /** Last segment of path*/
  def name: String = if (path == fileSystem.separator) path else jpath.getFileName.toString

  /**Last segment, extension removed*/
  def simpleName: String = {
    if (extension == None)
      name
    else
      name.dropRight(name.size - name.lastIndexWhere(_ == '.'))
  }

  /**Extension, ignores leading dot on hidden files*/
  def extension: Option[String] = {
    if (name == ".." || path == "" || name.tail.count(_ == '.') == 0)
      None
    else
      Some(name.drop(name.lastIndexWhere(_ == '.') + 1))
  }

  /**Returns sibling with extension, replacing existing one if necessary*/
  def withExtension(ext: Option[String]): Path = {
    if (ext != None)
      sibling(simpleName + "." + ext)
    else
      this
  }

  /**Returns segments, including root*/
  def segments: Seq[Path] = segmentIterator.toSeq

  /**Just like segments */
  def segmentIterator: Iterator[Path] = {
    if (isAbsolute)
      Iterator(fileSystem.path(fileSystem.separator)) ++ jpath.iterator().asScala.map(Path(_))
    else
      jpath.iterator().asScala.map(Path(_))
  }

  def segmentCount: Int = if (isAbsolute) jpath.getNameCount + 1 else jpath.getNameCount

  private def optPath(p: JPath): Option[Path] = if (p == null) None else Some(new Path(p))

  def root: Option[Path] = optPath(jpath.getRoot)

  def parent: Option[Path] = optPath(jpath.getParent)

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
  def resolve(other: Path): Path = {
    if (other.isAbsolute)
      Path(jpath.resolve(other.relativeTo(other.root.get).jpath))
    else
      Path(jpath.resolve(other.jpath))
  }

  /**Returns this concatenated with other.  If other is absolute, other is relativized first*/
  def resolve(other: String): Path = resolve(fileSystem.path(other))

  /**Returns this resolved with other*/
  def / (other: Path): Path = resolve(other)

  /**Returns this resolved with other*/
  def / (other: String): Path = resolve(other)

  /**Returns other concatenated onto your parent
    *
    * If you have no parent other is concatenated with the empty path.  Root's
    * siblings are an error.*/
  def sibling(other: Path): Path = {
    if(root != None)
      if (this == root.get)
        throw new IOException("Root has no sibling")
   if (parent == None)
    fileSystem.path("").resolve(other)
   else
    parent.get.resolve(other)
  }

  /**Returns other concatenated onto your parent*/
  def sibling(other: String): Path = sibling(fileSystem.path(other))

  //--------------------------------------------------------------------------------------------------------------------

  /** Returns the jpath after making it a real path */
  def toRealPath(options: LinkOption*): Path = Path(jpath.toRealPath(options : _*))

  /** Returns true if the jpath exists */
  def exists(options: LinkOption*): Boolean = Files.exists(jpath, options:_*)

  /** Returns true if the jpath does not exist */
  def nonExistent(options: LinkOption*): Boolean = Files.notExists(jpath, options:_*)

  /** Returns true if the jpath is the same as the argument path */
  def isSame(other: Path): Boolean = Files.isSameFile(jpath, other.jpath)

  /** Returns the size of the file pointed to by jpath */
  def size(): Option[Long] = {
    try{
      Option(Files.size(jpath))
    }
    catch {
      case e: NoSuchFileException => None
    }
  }

  /** Returns true if the jpath is a directory */
  def isDirectory(options: LinkOption*): Boolean = Files.isDirectory(jpath, options: _*)

  /** Returns true if the jpath is a regular file */
  def isFile(options: LinkOption*): Boolean = Files.isRegularFile(jpath, options: _*)

  /** Returns true if the jpath is a symbolic link */
  def isSymLink(): Boolean = Files.isSymbolicLink(jpath)

  /** Returns true if the jpath is a hidden file */
  def isHidden(): Boolean = Files.isHidden(jpath)

  /** Returns true if the jpath is readable */
  def isReadable(): Boolean = Files.isReadable(jpath)

  /** Returns true if the jpath is writable */
  def isWritable(): Boolean = Files.isWritable(jpath)

  /** Returns true if the jpath is executable */
  def isExecutable(): Boolean = Files.isExecutable(jpath)

  /** Check access modes for the underlying path -> execute, read and write */
  def checkAccess(modes: AccessMode*): Boolean = {
    val accessAttempt: Try[Unit] = Try(fileSystem.provider.checkAccess(jpath, modes.toSeq:_*))
    accessAttempt match {
      case accessible: Success[Unit] => true
      case notAccessible: Failure[Unit] => false
    }
  }

  /** Returns the last modified time */
  def lastModified(): FileTime = Files.getLastModifiedTime(jpath)

  /** Sets POSIX file permissions according to the arguments */
  def posixFilePerm_=(perms: Set[PosixFilePermission]) : Path = Path(Files.setPosixFilePermissions(jpath, perms.asJava))

  /** Gets POSIX file permissions*/
  def posixFilePerm(options: LinkOption*) : Set[PosixFilePermission] = Files.getPosixFilePermissions(jpath, options:_*).asScala.toSeq.toSet

  /** Creates a file */
  def createFile(createParents: Boolean = true, failIfExists: Boolean = true,
                 attributes: TraversableOnce[FileAttribute[_]] = Nil): this.type = {
    if(exists() && failIfExists)
      throw new IOException("File already exists")
    else if(isDirectory())
      throw new IOException("Path is a directory hence cannot be created as a file. Use createDirectory instead")

    if(createParents)
      createParentDirs()
    Files.createFile(jpath, attributes.toSeq:_*)
    this
  }

  /** Creates a directory */
  def createDirectory(createParents: Boolean = true, failIfExists: Boolean = true,
                      attributes: TraversableOnce[FileAttribute[_]] = Nil): this.type = {
    if(exists() && failIfExists)
      throw new IOException("Directory already exists")
    else if(isFile())
      throw new IOException("Path is a file hence cannot be created as a directory. Use createFile instead")
    if(createParents && (this != root.get))
      createParentDirs()
    if(this != root.get && nonExistent())
      Files.createDirectory(jpath, attributes.toSeq:_*)
    this
  }

  def createParentDirs(): this.type = {
    parent.foreach(_.createDirectory(createParents = true, failIfExists = false))
    this
  }

  /** Deletes a file if and only if it exists*/
  def deleteIfExists(): Boolean = Files.deleteIfExists(jpath)

  /** Deletes a file if it exists*/
  def delete(): Unit = Files.delete(jpath)

  /** Recursively deletes a directory and all of it's contents.
   *
   * Using the walkFileTree method, the function will recursively walk the target file tree
   * and delete every element.
  */
  def deleteRecursively(): Boolean = {
    if(isDirectory()) {
      Files.walkFileTree(jpath,
        new SimpleFileVisitor[JPath] {
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

  /** Copies a file to the target location */
  def copyTo(target: Path, options: CopyOption*): Path = Path(Files.copy(jpath, target.jpath, options:_*))


  /** Moves a file to the target location */
  def moveFile(target: Path, options: CopyOption*): Unit = Files.move(jpath, target.jpath, options:_*)

  /** Moves a directory to a given path recursively
   *
   * Uses Files walkFileTree method to recursively copy the entire
   * contents of a directory to the target location
  */
  def moveDirectory(target: Path) {
    if(isDirectory()) {
      Files.walkFileTree(jpath,
        new SimpleFileVisitor[JPath] {
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

  //--------------------------------------------------------------------------------------------------------------------

  /** Opens and returns a new input stream that will read from this path. */
  def inputStream(options: OpenOption*): InputStream = Files.newInputStream(jpath, options: _*)

  /** Opens and returns a new output stream that will write to this path. */
  def outputStream(options: OpenOption*): OutputStream = Files.newOutputStream(jpath, options: _*)

}
