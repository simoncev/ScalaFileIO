package com.rgm.file

/**
 * Created by thausler on 6/6/14.
 */

import java.nio.file.{FileSystem => JFileSystem, WatchService, FileStore}
import scala.collection.JavaConverters._
import java.nio.file.attribute.UserPrincipalLookupService
import java.nio.file.spi.FileSystemProvider

object FileSystem {
  def apply(jsystem: JFileSystem): FileSystem = new FileSystem(jsystem)

  //add implicit default
}

class FileSystem(jsystem : JFileSystem) {

  def close = jsystem.close

  def fileStores : Iterable[FileStore] = jsystem.getFileStores.asScala

  def path (segments : String *) : Path = Path(jsystem.getPath(segments.head, segments.tail : _*))

  def pathFinder (syntaxAndPattern : String ): PathFinder = ???

  def separator : String = jsystem.getSeparator

  def userPrincipalLookupService : UserPrincipalLookupService = jsystem.getUserPrincipalLookupService

  def isOpen : Boolean = jsystem.isOpen

  def isReadOnly : Boolean = jsystem.isReadOnly

  def newWatchService: WatchService = jsystem.newWatchService

  def provider : FileSystemProvider = jsystem.provider

  def supportedFileAttributeViews : Set[String] = ???

  //etc...including scalax FileSystem functionality
}

