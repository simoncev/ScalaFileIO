package com.rgm


import java.nio.file.{FileSystem => JFileSystem, WatchService, FileStore}
import scala.collection.JavaConverters._
import java.nio.file.attribute.UserPrincipalLookupService
import java.nio.file.spi.FileSystemProvider

package object file {

  // add a wrapper class for this (with a scala-like interface) instead of just an alias
  object FileSystem {
    def apply(jsystem: JFileSystem): FileSystem = new FileSystem(jsystem)


  }

  class FileSystem(jsystem : JFileSystem) {

    def close = jsystem.close

    def fileStores : Iterable[FileStore] = jsystem.getFileStores.asScala

    def path (segments : String *) : Path = jsystem.getPath(segments.head, segments.tail : _*)


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

}
