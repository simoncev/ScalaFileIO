package com.rgm.file

import java.io.Closeable
import java.nio.file.attribute.UserPrincipalLookupService
import java.nio.file.spi.FileSystemProvider
import java.nio.file.{FileStore, FileSystems, WatchService, FileSystem => JFileSystem}
import scala.collection.JavaConverters._

object FileSystem {
  def apply(jsystem: JFileSystem): FileSystem = new FileSystem(jsystem)

  implicit val default: FileSystem = FileSystem(FileSystems.getDefault)
}

class FileSystem(jsystem : JFileSystem) extends Closeable {

  def close() = jsystem.close()

  def fileStores: Iterable[FileStore] = jsystem.getFileStores.asScala

  def path(segments: String*): Path = Path(jsystem.getPath(segments.head, segments.tail: _*))

  def pathMatcher(syntaxAndPattern: String): PathMatcher = PathMatcher.fromJava(jsystem.getPathMatcher(syntaxAndPattern))

  def separator: String = jsystem.getSeparator

  def userPrincipalLookupService: UserPrincipalLookupService = jsystem.getUserPrincipalLookupService

  def isOpen: Boolean = jsystem.isOpen

  def isReadOnly: Boolean = jsystem.isReadOnly

  def newWatchService: WatchService = jsystem.newWatchService

  def provider: FileSystemProvider = jsystem.provider

  def supportedFileAttributeViews: Set[String] = ???

  //etc...including scalax FileSystem functionality
}
