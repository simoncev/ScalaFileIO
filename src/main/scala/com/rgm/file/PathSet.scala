package com.rgm.file

import java.nio.file.{SimpleFileVisitor, LinkOption, Files}
import java.nio.file.{Path => JPath, FileSystem => JFileSystem, _}
import scala.collection.mutable.ListBuffer
import java.nio.file.attribute._
import java.io.{File => JFile, IOException}
import collection.mutable.HashMap
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
object PathSet {

  def apply(path: Path, paths: Path*): PathSet = {
    if (paths.isEmpty)
      new SimplePathSet(path)
    else {
      var result: PathSet = new SimplePathSet(path)
      for (p <- paths) {
        result = new CompoundPathSet(result, new SimplePathSet(p))//replace with +++?
      }
      result
    }
  }
}

abstract class PathSet extends Traversable[Path] {
  def +++(includes: PathSet): PathSet = new CompoundPathSet(this, includes)


  def ---(excludes: PathSet): PathSet = {
    var result = this.toList
    for(i <- excludes)
      result = result diff List(i)
    var r: PathSet = new SimplePathSet(result.head)
    for (p <- result.tail)
      r = new CompoundPathSet(r, new SimplePathSet(p))//replace with +++?
    r
  }

  def *(matcher: PathMatcher): PathSet = new FilteredPathSet(this, 1, matcher)

  def **(matcher: PathMatcher, d: Int): PathSet = new FilteredPathSet(this, d, matcher)

  def *** : PathSet = this ** (PathMatcher(""".*""".r), Int.MaxValue)

  def /(literal: String): PathSet = this ** (PathMatcher(literal),1)
}

class SimplePathSet(root: Path) extends PathSet {
  private var memberPath: Path = root

  override def foreach[U](f: Path => U) = {
    f(memberPath)
  }
}

final class FilteredPathSet(p: PathSet, depth: Int, matcher: PathMatcher) extends PathSet {
  private var memberPathSet: PathSet = p

  override def foreach[U](f: Path => U) = {
    var d: Int = depth
    for (root <- p) {
      if (root.exists()) {
        Files.walkFileTree(root.jpath,
          new SimpleFileVisitor[JPath] {
            override def preVisitDirectory(dir: JPath, attrs: BasicFileAttributes): FileVisitResult = {
              if (d <= 0) {
                return FileVisitResult.SKIP_SUBTREE
              }
              else {
                if (matcher.matches(Path(dir))) {
                  f(com.rgm.file.Path(dir))
                }
                d -= 1
                FileVisitResult.CONTINUE
              }
            }

            override def visitFile(file: JPath, attrs: BasicFileAttributes): FileVisitResult = {
              if (matcher.matches(Path(file)))
                f(com.rgm.file.Path(file))
              FileVisitResult.CONTINUE
            }

            override def postVisitDirectory(dir: JPath, e: IOException): FileVisitResult = {
              d += 1
              FileVisitResult.CONTINUE
            }
          })
      }
    }
  }
}



final class CompoundPathSet(pathSet1: PathSet, pathSet2: PathSet) extends PathSet {


  override def foreach[U](f: Path => U) = {
    pathSet1.foreach(f)
    pathSet2.foreach(f)
  }
}

