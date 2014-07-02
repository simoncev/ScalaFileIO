package com.rgm.file

import java.nio.file.{SimpleFileVisitor, LinkOption, Files}
import java.nio.file.{Path => JPath, FileSystem => JFileSystem, _}
import java.nio.file.attribute._
import java.io.{File => JFile, IOException}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection.{GenTraversableOnce, TraversableViewLike, TraversableView}
import com.rgm.file.PathSet.PathSetCanBuildFrom

/**
 * Created by sshivaprasad on 6/18/14.
 */

object PathSet {

  def apply(paths: Path*): PathSet = {
    new SimplePathSet(paths: _*)
  }

  implicit def canBuildFrom: CanBuildFrom[Traversable[Path], Path, PathSet] =
    new PathSetCanBuildFrom

//
//  protected class PathSetBuilder(pathSet: PathSet) extends Builder[Path,PathSet] {
//    def result() = pathSet
//    def clear() = super.clear()
//    def +=(p: Path) = super.+=(p)
//  }

  protected class MappedPathSetBuilder(pathSet: PathSet, f: Path=>Path) extends Builder[Path,PathSet] {
    def result() = new MappedPathSet(pathSet, f)
    def clear() = this
    def +=(p: Path) = this
  }

  protected class PathSetCanBuildFrom extends CanBuildFrom[Traversable[Path], Path, PathSet] {
      def apply(): Builder[Path,PathSet] = new MappedPathSetBuilder(new SimplePathSet, (p: Path) => p)
      def apply(pathSet: Traversable[Path]) = new MappedPathSetBuilder(pathSet.asInstanceOf[PathSet], (p: Path) => p)
      //only one intended for use
      def apply(pathSet: PathSet, f: Path=>Path) = new MappedPathSetBuilder(pathSet, f)
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

  def *(matcher: PathMatcher): PathSet = new TreeWalkPathSet(this, 1, matcher)

  def **(matcher: PathMatcher): PathSet = new TreeWalkPathSet(this, Int.MaxValue, matcher)

  def **(matcher: PathMatcher, d: Int): PathSet = new TreeWalkPathSet(this, d, matcher)

  def *** : PathSet = this **(PathMatcher( """.*""".r), Int.MaxValue)

  def /(literal: String): PathSet = this **(PathMatcher(literal), 1)

  def ancestorsOf(p: Path): Set[Path]

  protected def underlying: PathSet = this

  override def map[B, That](f: Path => B)(implicit bf: CanBuildFrom[Traversable[Path], B, That]): That = {
    println(bf)
    bf match {
      case psbf: PathSetCanBuildFrom => psbf(this, f.asInstanceOf[Path => Path]).result().asInstanceOf[That]
      case _ => super.map(f)
    }
  }

  override def filter(p: Path => Boolean): PathSet = {
    new FilteredPathSet(this, p)
  }

  //
//  override def flatMap[B, That](f: Path => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Traversable[Path], B, That]): That = {
//

//  override def withFilter(p: Path => Boolean): PathSet = {
//    new WithFilteredPathSet(this,p)
//  }




}


//final private class WithFilteredPathSet(p: PathSet, func: Path => Boolean) extends PathSet {
//  override def foreach[U](f: Path => U) = {
//    p.foreach((p: Path) => if (func(p)) f(p))
//  }
//
//  override def ancestorsOf(i: Path): Set[Path] = {
//    p.ancestorsOf(i)
//  }
//
//  override def map[B, That](f: Path => B)(implicit bf: CanBuildFrom[PathSet, Path, PathSet]): That = {
//    val ps: PathSet = new SimplePathSet()
//    val b = bf(this)
//    //for (x <- this.init)
//    p.foreach((pth: Path) => if (func(pth)) b += f(pth) )
//    b
//  }
//
//  override def withFilter(q: Path => Boolean): WithFilter = new WithFilter(x => func(x) && q(x))
//
//  def foreach[U](f: Path => U): Unit =
//    for (x <- this)
//      if (func(x)) f(x)
//
//
//}

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
    for (i <- pathSetSeq) i.foreach(f)
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

  override def ancestorsOf(i: Path): Set[Path] = {
    val aAnc: Set[Path] = superset.ancestorsOf(i)
    val bAnc: Set[Path] = excluded.ancestorsOf(i)
    aAnc -- bAnc
  }
}

final private class FilteredPathSet(p: PathSet, func: Path => Boolean) extends PathSet {
  override def foreach[U](f: Path => U) = {
    p.foreach((p: Path) => if(func(p)) f(p))
  }

  override def ancestorsOf(i: Path): Set[Path] = {
    p.ancestorsOf(i)
  }
}

final private class TreeWalkPathSet(memberPathSet: PathSet, depth: Int, matcher: PathMatcher) extends PathSet {

  override def foreach[U](f: Path => U) = {
    var d: Int = depth
    for (root <- memberPathSet) {
      if (root.exists()) {
        Files.walkFileTree(root.jpath,
          new SimpleFileVisitor[JPath] {
            override def preVisitDirectory(dir: JPath, attrs: BasicFileAttributes): FileVisitResult = {
              if (matcher.matches(Path(dir)) && !(root == Path(dir)))
                f(Path(dir))
              if (d == 0)
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
      if (p startsWith root) {
        val deepestPath: Int = if (root.segmentCount + depth < 0) Int.MaxValue else root.segmentCount + depth
        for (n <- root.segmentCount + 1 to Math.min(deepestPath, p.segmentCount)) {
          val candidateAncestor = Path(p.segments.slice(0, n).mkString(p.fileSystem.separator))
          if (matcher.matches(candidateAncestor)) {
            ancestorSet += candidateAncestor
          }
        }
      }
    }
    ancestorSet
  }
}

final class MappedPathSet(pathSet: PathSet, func: Path => Path) extends PathSet {

  override def foreach[U](f: Path => U) = {
    for (p <- pathSet)
      f(p)
  }
  override def ancestorsOf(p: Path): Set[Path] = {
    var ancestorSet = Set[Path]()
    for (candidateAncestor <- pathSet)
      if (p startsWith func(candidateAncestor))
        ancestorSet += func(candidateAncestor)
    ancestorSet
  }

}
