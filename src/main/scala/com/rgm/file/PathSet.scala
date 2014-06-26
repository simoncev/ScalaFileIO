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
    (this, includes) match {
      case (xThis: SimplePathSet, xIncludes: SimplePathSet) => {
        new SimplePathSet((xThis.root ++ xIncludes.root): _*)
      }
      case (xThis: CompoundPathSet, xIncludes: CompoundPathSet) => {
        new CompoundPathSet((xThis.pathSetSeq ++ xIncludes.pathSetSeq): _*)
      }
      case (xThis: PathSet, xIncludes: CompoundPathSet) => {
        new CompoundPathSet((Seq(xThis) ++ xIncludes.pathSetSeq): _*)
      }
      case (xThis: CompoundPathSet, xIncludes: PathSet) => {
        new CompoundPathSet((xThis.pathSetSeq ++ Seq(xIncludes)): _*)
      }
      case (xThis: PathSet, xIncludes: PathSet) => {
        new CompoundPathSet(Seq(xThis) ++ Seq(xIncludes): _*)
      }
    }
  }

  def ---(excludes: PathSet): PathSet = new ExclusionPathSet(this, excludes)

  def *(matcher: PathMatcher): PathSet = new FilteredPathSet(this, 1, matcher)

  def **(matcher: PathMatcher): PathSet = new FilteredPathSet(this, Int.MaxValue, matcher)

  def **(matcher: PathMatcher, d: Int): PathSet = new FilteredPathSet(this, d, matcher)

  def *** : PathSet = this **(PathMatcher( """.*""".r), Int.MaxValue)

  def /(literal: String): PathSet = this **(PathMatcher(literal), 1)

  def ancestorsOf(p: Path): Set[Path]
}

final class SimplePathSet(roots: Path*) extends PathSet {
  val root: Seq[Path] = roots
  override def foreach[U](f: Path => U) = {
    roots.foreach((p: Path) => if (p.exists()) f(p))
  }

  override def ancestorsOf(i: Path) : Set[Path] = {
    var result: Set[Path] = Set[Path]()
    for(p <- root) {
      if(i startsWith p)
        result += p
    }
    result
  }
}

final private class CompoundPathSet(pathSets: PathSet*) extends PathSet {

  val pathSetSeq: Seq[PathSet] = pathSets

  override def foreach[U](f: Path => U) = {
    for (i <- pathSets) i.foreach(f)
  }

  override def ancestorsOf(p: Path): Set[Path] = {
    var ancestorSet = Set[Path]()
    for (pathSet <- pathSetSeq) {
      ancestorSet = ancestorSet ++ pathSet.ancestorsOf(p)
    }
    ancestorSet

  }
}

final private class ExclusionPathSet(superset: PathSet, excluded: PathSet) extends PathSet {

  override def foreach[U](f: Path => U) = {
    superset.foreach((p: Path) => if (!excluded.ancestorsOf(p).contains(p)) f(p))
  }

  override def ancestorsOf(i: Path) = {
    val aAnc: Set[Path] = superset.ancestorsOf(i)
    val bAnc: Set[Path] = excluded.ancestorsOf(i)
    aAnc -- bAnc
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

  override def ancestorsOf(p: Path): Set[Path] = {
    val ancestorRoots = memberPathSet.ancestorsOf(p)
    var ancestorSet = Set[Path]()
    for (root <- ancestorRoots) {
      if (p startsWith root)
        for(n <- 1 to Math.min(depth, p.segmentCount)) {
          val candidateAncestor = Path(p.segments.slice(0, root.segmentCount + n).mkString("/"))
          if (matcher.matches(candidateAncestor))
            ancestorSet += candidateAncestor
          else
            println("rejected: " + candidateAncestor)//REMOVE
        }
    }
    ancestorSet
  }
}
