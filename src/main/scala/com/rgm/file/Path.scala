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

  /** Complete path
    * @return Path in string format
    * */
  def path: String = jpath.toString

  /** Name of the file or directory including extension
   * @return Last segment of path, that is either a file or directory
   * */
  def name: String = if (path == fileSystem.separator) path else jpath.getFileName.toString

  /** Name of the file or directory excluding extension
   * @return The last segment of the path (either file or director) without the extension
   * */
  def simpleName: String = {
    if (extension == None)
      name
    else
      name.dropRight(name.size - name.lastIndexWhere(_ == '.'))
  }

  /** Extension, ignores leading dot on hidden files
    * @return The extension of the file or directory, ignores leading dot on hidden files
    * */
  def extension: Option[String] = {
    if (name == ".." || path == "" || name.tail.count(_ == '.') == 0)
      None
    else
      Some(name.drop(name.lastIndexWhere(_ == '.') + 1))
  }

  /** Finds a sibling along with its extension
    * @return Sibling with extension, replacing existing one if necessary
    * @param ext
    *            An optional string to add as extension
    * */
  def withExtension(ext: Option[String]): Path = {
    if (ext != None)
      sibling(simpleName + "." + ext)
    else
      this
  }

  /** Segments the underlying jpath including the root
    * @return A sequence consisting of the path, split based on "/".
    *         NOTE: Root or "/" is included in the sequence
    * */
  def segments: Seq[Path] = segmentIterator.toSeq

  /**Just like segments, return type is different
    * @return Iterator over the segments of the underlying path
    * */
  def segmentIterator: Iterator[Path] = {
    if (isAbsolute)
      Iterator(fileSystem.path(fileSystem.separator)) ++ jpath.iterator().asScala.map(Path(_))
    else
      jpath.iterator().asScala.map(Path(_))
  }

  /** Counts the number of segments in the path
    * @return An integer representing the number of segments
    * */
  def segmentCount: Int = if (isAbsolute) jpath.getNameCount + 1 else jpath.getNameCount

  private def optPath(p: JPath): Option[Path] = if (p == null) None else Some(new Path(p))

  /** Finds the root of the current system
    * @return Path to the root of the system
    * */
  def root: Option[Path] = optPath(jpath.getRoot)

  /** The parent segment if possible. Root does not have a parent
    * @return Path to the parent
    * */
  def parent: Option[Path] = optPath(jpath.getParent)

  /** Subpath starting at begin, stopping at end
    * @param begin
    *             Point where the subpath must begin
    * @param end
    *             Point where the subpath must end
    * @return Path following the argument constraints
    * */
  def subpath(begin: Int, end: Int): Path = Path(jpath.subpath(begin, end))

  /** Returns true if this starts with other
    * @param other
    *             Start path to check against
    * @return True if the path starts with other, false otherwise
    * */
  def startsWith(other: Path): Boolean = jpath.startsWith(other.jpath)

  /** Returns true if this starts with other
    * @param other
    *              Start path to check against, string type
    * @return True if the path starts with other, false otherwise
    * */
  def startsWith(other: String): Boolean = jpath.startsWith(fileSystem.path(other).jpath)

  /** Returns true if this ends with other
    * @param other
    *               End path to check against
    * @return True if underlying path ends with other
    * */
  def endsWith(other: Path): Boolean = jpath.endsWith(other.jpath)

    /** String argument version of above function*/
  def endsWith(other: String): Boolean = jpath.endsWith(fileSystem.path(other).jpath)

  /** Checks if the underlying path is absolute
    * @return True if the underlying path is absolute
    * */
  def isAbsolute: Boolean = jpath.isAbsolute

  /**Converts a path to its absolute form
    * @return A path of absolute form
    * */
  def toAbsolute: Path = Path(jpath.toAbsolutePath)

  /** Removes redundancies in the path.  Empty path maps to empty path
    * @return A path with redundancies removed
    * */
  def normalize: Path =
    if (jpath.toString.equals(""))
      fileSystem.path("")
    else
      Path(jpath.normalize)

  /** Convert a path to URI
    * @return A URI version of the underlying path
    * */
  def toURI: URI = jpath.toUri

  /** Converts a path to URL
    * @return A URL verion of the underlying path
    * */
  def toURL: URL = toURI.toURL

  /** Converts a path to a Java File object
    * @return A Java file object
    * */
  def jfile: JFile = jpath.toFile

  /**Relativizes from other to this
    * @param other
    *              The path to relativize from
    * @return A path that has been relativized
    * */
  def relativize(other: Path): Path = Path(jpath.relativize(other.jpath))

  /** Same as the above, except it takes String argument */
  def relativize(other: String): Path = Path(jpath.relativize(fileSystem.path(other).jpath))

  /**Relativize from this to base
    * @param base
    *             The path to relativize to
    * @return A path after it has been relativized
    * */
  def relativeTo(base: Path): Path = base.relativize(this)

  /** Same as above function except it takes a String argument */
  def relativeTo(base: String): Path = fileSystem.path(base).relativize(this)

  /** . Resolves against the other path argument. If other is absolute, other is relativized first
    * @param other
    *              The path to resolve to
    * @return This concatenated with other
    * */
  def resolve(other: Path): Path = {
    if (other.isAbsolute)
      Path(jpath.resolve(other.relativeTo(other.root.get).jpath))
    else
      Path(jpath.resolve(other.jpath))
  }

  /** Same as the above function, except it takes a String argument*/
  def resolve(other: String): Path = resolve(fileSystem.path(other))

  /** Resolves a path
    * @param other
    *              Path to resolve with
    * @return This resolved with other
    * */
  def / (other: Path): Path = resolve(other)

  /**Same as above takes a String argument*/
  def / (other: String): Path = resolve(other)

  /** Creates a sibling of This and other
    * NOTE: If you have no parent other is concatenated with the empty path.  Root's
    * siblings are an error.
    * @param other
    *              The path to concatenate
    * @return other concatenated onto your parent
    * */
  def sibling(other: Path): Path = {
    if(root != None)
      if (this == root.get)
        throw new IOException("Root has no sibling")
   if (parent == None)
    fileSystem.path("").resolve(other)
   else
    parent.get.resolve(other)
  }

  /** Saem as the above function, except argument is String*/
  def sibling(other: String): Path = sibling(fileSystem.path(other))

  //--------------------------------------------------------------------------------------------------------------------

  /** Converts path to a real path
    * @param options
    *                Linking options
    * @return the jpath after making it a real path
    * */
  def toRealPath(options: LinkOption*): Path = Path(jpath.toRealPath(options : _*))

  /** Check if the Path exists
    * @param options
    *                Linking options
    * @return True if the jpath exists
    * */
  def exists(options: LinkOption*): Boolean = Files.exists(jpath, options:_*)

  /** Checks for non existence
    * @param options
    *                Linking options
    * @return True if the jpath does not exist
    * */
  def nonExistent(options: LinkOption*): Boolean = Files.notExists(jpath, options:_*)

  /** Checks if the paths are the same
    * @param other
    *              The path to compare against
    * @return True if the jpath is the same as the argument path
    * */
  def isSame(other: Path): Boolean = Files.isSameFile(jpath, other.jpath)

  /** The size of the file
    * @return the size of the file pointed to by jpath
    * */
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

  /** Check access modes for the underlying path -> execute, read and write
    * @param modes
    *              A set of accessmodes that need to be checked against
    * @return True if modes provided are met
    * */
  def checkAccess(modes: AccessMode*): Boolean = {
      Try(fileSystem.provider.checkAccess(jpath, modes.toSeq:_*)) match {
      case accessible: Success[Unit] => true
      case notAccessible: Failure[Unit] => false
    }
  }

  /** Returns the last modified time */
  def lastModified(): FileTime = Files.getLastModifiedTime(jpath)

  /** Sets POSIX file permissions according to the arguments. This is the setter method.
    * @param perms
    *              A set of PosixFilePermission's to which the file must be set
    * @return The path to the file whos permission have been set
    * */
  def posixFilePerm_=(perms: Set[PosixFilePermission]) : Path = Path(Files.setPosixFilePermissions(jpath, perms.asJava))

  /** Gets POSIX file permissions. This is the getter method.
    * @param options
    *                Linking options
    * @return A set that includes all the file permissions of the file represented by the underlying path
    * */
  def posixFilePerm(options: LinkOption*) : Set[PosixFilePermission] = Files.getPosixFilePermissions(jpath, options:_*).asScala.toSeq.toSet

  /** Creates a file
    * @param createParents
    *                      If set to true then all parent directories that do not exist will be created
    * @param failIfExists
    *                      If set to true then it will fail is the file already exists on disk
    * @param attributes
    *                      Attributes about the file that would like to be set on creation
    * @return A path to the created file
    * */
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

  /** Creates a directory
    * @param createParents
    *                      If set to true then all parent directories that do not exist will be created
    * @param failIfExists
    *                      If set to true then it will fail is the file already exists on disk
    * @param attributes
    *                      Attributes about the directory that would like to be set on creation
    * @return A path to the created director
    * */
  def createDirectory(createParents: Boolean = true, failIfExists: Boolean = true,
                      attributes: TraversableOnce[FileAttribute[_]] = Nil): this.type = {
    val dirExists = exists()
    if(dirExists && failIfExists)
      throw new IOException("Directory already exists")
    else if(isFile())
      throw new IOException("Path is a file hence cannot be created as a directory. Use createFile instead")
    if(createParents)
      createParentDirs()
    if(!dirExists)
      Files.createDirectory(jpath, attributes.toSeq:_*)
    this
  }

  private def createParentDirs(): this.type = {
    parent.foreach(_.createDirectory(createParents = true, failIfExists = false))
    this
  }

  /** Deletes a file if and only if it exists
    * @return True if it existed and was deleted successfully
    * */
  def deleteIfExists(): Boolean = Files.deleteIfExists(jpath)

  /** Deletes a file
    * @return True if the file was deleted
    * */
  def delete(): Unit = Files.delete(jpath)

  /** Recursively deletes a directory and all of it's contents.
   *
   * Using the walkFileTree method, the function will recursively walk the target file tree
   * and delete every element.
   *
   * @return True if the recursive delete was successful
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

  /** Copies a file to the target location
    * @param target
    *               This is the target path to where the file will be copied
    * @param options
    *                Copy options
    * @return Path to the copied file
    * */
  def copyTo(target: Path, options: CopyOption*): Path = Path(Files.copy(jpath, target.jpath, options:_*))


  /** Moves a file to the target location
    * @param target
    *               Target location to where file will be moved
    * @param options
    *                Copy options
    *
    * */
  def moveFile(target: Path, options: CopyOption*): Unit = Files.move(jpath, target.jpath, options:_*)

  /** Moves a directory to a given path recursively
   *
   * Uses Files walkFileTree method to recursively copy the entire
   * contents of a directory to the target location
    *
    * @param target
    *               Target location to where the directory must be recursively moved
  */
  def moveDirectory(target: Path): Unit  = {
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

  /** Opens and returns a new input stream that will read from this path.
    * @param options
    *                Open options
    * @return An open inputstream connection
    * */
  def inputStream(options: OpenOption*): InputStream = {
    Try(Files.newInputStream(jpath, options: _*)) match {
      case success: Success[InputStream] => success.get
      case fail: Failure[InputStream] => throw new java.io.FileNotFoundException
    }
  }

  /** Opens and returns a new output stream that will write to this path.
    * @param options
    *               Open options
    * @return An open output stream connection
    * */
  def outputStream(options: OpenOption*): OutputStream = {
    Try(Files.newOutputStream(jpath, options: _*)) match {
      case success: Success[OutputStream] => success.get
      case fail: Failure[OutputStream] => throw new java.io.FileNotFoundException
    }
  }

}
