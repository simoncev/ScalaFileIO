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

object PathSet {

  def apply(path: Path, paths: Path*): PathSet = {
    if (paths.isEmpty)
      new SimplePathSet(path)
    else {
      var result: PathSet = new SimplePathSet(path)
      for (p <- paths) {
        result = result +++ new SimplePathSet(p)
      }
      result
    }
  }
}

abstract class PathSet extends Traversable[Path] {
  def +++(includes: PathSet): PathSet = new CompoundPathSet(this, includes)

  def ---(excludes: PathSet): PathSet = new ExclusionPathSet(this, excludes)

  def *(matcher: PathMatcher): PathSet = new FilteredPathSet(this, 1, matcher)

  def **(matcher: PathMatcher): PathSet = new FilteredPathSet(this, Int.MaxValue, matcher)

  def **(matcher: PathMatcher, d: Int): PathSet = new FilteredPathSet(this, d, matcher)

  def *** : PathSet = this ** (PathMatcher(""".*""".r), Int.MaxValue)

  def /(literal: String): PathSet = this ** (PathMatcher(literal),1)
}

final class SimplePathSet(root: Path) extends PathSet {

  override def foreach[U](f: Path => U) = {
    f(root)
  }
}

final private class FilteredPathSet(memberPathSet: PathSet, depth: Int, matcher: PathMatcher) extends PathSet {

  override def foreach[U](f: Path => U) = {
    var d: Int = depth
    for (root <- memberPathSet) {
      if (root.exists()) {
        Files.walkFileTree(root.jpath,
          new SimpleFileVisitor[JPath] {
            override def preVisitDirectory(dir: JPath, attrs: BasicFileAttributes): FileVisitResult = {
              if (d < 0)
                return FileVisitResult.SKIP_SUBTREE
              else if(d == 0) {
                if (matcher.matches(Path(dir)) && !(root == Path(dir)))
                  f(Path(dir))
                return FileVisitResult.SKIP_SUBTREE
              }
              else {
                if (matcher.matches(Path(dir)) && !(root == Path(dir)))
                  f(Path(dir))
                d -= 1
                FileVisitResult.CONTINUE
              }
            }

            override def visitFile(file: JPath, attrs: BasicFileAttributes): FileVisitResult = {
              if (matcher.matches(Path(file)) && !(root == Path(file)))
                f(Path(file))
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



final private class CompoundPathSet(pathSet: PathSet*) extends PathSet {


  override def foreach[U](f: Path => U) = {
    for(i <- pathSet) i.foreach(f)
  }
}

final private class ExclusionPathSet(superset: PathSet, excluded: PathSet) extends PathSet {

  override def foreach[U](f: Path => U) = {
    val excludees = excluded.toList
    superset.foreach((p: Path) => if (!excludees.contains(p)) f(p))
  }
}

