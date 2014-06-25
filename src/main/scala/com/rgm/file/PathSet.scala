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

  def apply(paths: Path*): PathSet = {
    new SimplePathSet(paths:_*)
  }
}

abstract class PathSet extends Traversable[Path] {
  def +++(includes: PathSet): PathSet = {
    (this,includes) match {
      case (xThis: SimplePathSet, xIncludes: SimplePathSet) => {
        new SimplePathSet((xThis.root ++ xIncludes.root):_*)
      }
      case (xThis: CompoundPathSet, xIncludes: CompoundPathSet) => {
        new CompoundPathSet((xThis.pathSet ++ xIncludes.pathSet):_*)
      }
      case (xThis: PathSet, xIncludes: CompoundPathSet) => {
        new CompoundPathSet((Seq(xThis) ++ xIncludes.pathSet):_*)
      }
      case (xThis: CompoundPathSet, xIncludes: PathSet) => {
        new CompoundPathSet((xThis.pathSet ++ Seq(xIncludes)):_*)
      }
      case (xThis: PathSet, xIncludes: PathSet) => {
        new CompoundPathSet(Seq(xThis) ++ Seq(xIncludes):_*)
      }
    }
  }

  def ---(excludes: PathSet): PathSet = new ExclusionPathSet(this, excludes)

  def *(matcher: PathMatcher): PathSet = new FilteredPathSet(this, 1, matcher)

  def **(matcher: PathMatcher): PathSet = new FilteredPathSet(this, Int.MaxValue, matcher)

  def **(matcher: PathMatcher, d: Int): PathSet = new FilteredPathSet(this, d, matcher)

  def *** : PathSet = this ** (PathMatcher(""".*""".r), Int.MaxValue)

  def /(literal: String): PathSet = this ** (PathMatcher(literal),1)
}

final class SimplePathSet(roots: Path*) extends PathSet {
  val root: Seq[Path] = roots
  override def foreach[U](f: Path => U) = {
    roots.foreach(f)
  }
}

final private class CompoundPathSet(pathSets: PathSet*) extends PathSet {
  val pathSet: Seq[PathSet] = pathSets
  override def foreach[U](f: Path => U) = {
    for(i <- pathSets) i.foreach(f)
  }
}

final private class ExclusionPathSet(superset: PathSet, excluded: PathSet) extends PathSet {

  override def foreach[U](f: Path => U) = {
    val excludees = excluded.toList
    superset.foreach((p: Path) => if (!excludees.contains(p)) f(p))
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
              if (matcher.matches(Path(dir)) && !(root == Path(dir)))
                f(Path(dir))
              if(d == 0)
                return FileVisitResult.SKIP_SUBTREE
              d -= 1
              FileVisitResult.CONTINUE
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