package com.rgm.file

import java.nio.file.{SimpleFileVisitor, LinkOption, Files}
import java.nio.file.{Path => JPath, FileSystem => JFileSystem, _}
import scala.collection.mutable.ListBuffer
import java.nio.file.attribute._
import java.io.{File => JFile, IOException}
import com.rgm.file.Path

/**
 * Created by sshivaprasad on 6/18/14.
 */


// class PathSet (root: com.rgm.file.Path, matcher: PathMatcher, maxDepth: Int, options: LinkOption*) extends Traversable[Path] {
//
//  override def foreach[U](f: (com.rgm.file.Path => U)): Unit = {
//    var d: Int = maxDepth
//    if(root.exists()) {
//      Files.walkFileTree(root.jpath,
//        new SimpleFileVisitor[JPath] {
//
//          override def preVisitDirectory(dir: JPath, attrs: BasicFileAttributes) : FileVisitResult = {
//            if(d <= 0) {
//              return FileVisitResult.SKIP_SUBTREE
//            }
//            else {
//              if(matcher.matches(Path(dir))) {
//                f(com.rgm.file.Path(dir))
//              }
//              d-=1
//              FileVisitResult.CONTINUE
//            }
//          }
//
//          override def visitFile(file: JPath,attrs: BasicFileAttributes ) : FileVisitResult = {
//            if(matcher.matches(Path(file)))
//              f(com.rgm.file.Path(file))
//            FileVisitResult.CONTINUE
//          }
//
//          override def postVisitDirectory(dir: JPath, e: IOException) : FileVisitResult = {
//            d+=1
//            FileVisitResult.CONTINUE
//          }
//        })
//    }
//  }
//}

class PathSet(root: Path) extends Traversable[Path] {
  private var memberPath: Path = root

  override def foreach[U](f: Path => U) {
    f(memberPath)
  }
}

final class FilteredPathSet(p: Path, depth: Int, m: PathMatcher) extends PathSet {
  private var memberPath1: Path
}

final class CompoundPathSet()
object PathSet {

  def apply(root: Path, matcher: PathMatcher, maxDepth: Int, options: LinkOption*): PathSet =
    new PathSet(root)

}
