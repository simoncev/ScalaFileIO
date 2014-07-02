package com.rgm.file

import java.nio.file.{SimpleFileVisitor, LinkOption, Files}
import java.nio.file.{Path => JPath, FileSystem => JFileSystem, _}
import java.nio.file.attribute._
import java.io.{File => JFile, IOException}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.collection.{TraversableLike, GenTraversableOnce, TraversableViewLike, TraversableView}
import com.rgm.file.PathSpec.PathSpecCanBuildFrom

/**
 * Created by sshivaprasad on 6/18/14.
 */

object PathSpec {

  def apply(paths: Path*): PathSpec = {
    new SimplePathSpec(paths: _*)
  }

  implicit def canBuildFrom: CanBuildFrom[Traversable[Path], Path, PathSpec] =
    new PathSpecCanBuildFrom

//
//  protected class PathSpecBuilder(pathSpec: PathSpec) extends Builder[Path,PathSpec] {
//    def result() = pathSpec
//    def clear() = super.clear()
//    def +=(p: Path) = super.+=(p)
//  }

  protected class MappedPathSpecBuilder(pathSpec: PathSpec, f: Path=>Path) extends Builder[Path,PathSpec] {
    def result() = new MappedPathSpec(pathSpec, f)
    def clear() = this
    def +=(p: Path) = this
  }

  protected class PathSpecCanBuildFrom extends CanBuildFrom[Traversable[Path], Path, PathSpec] {
      def apply(): Builder[Path,PathSpec] = new MappedPathSpecBuilder(new SimplePathSpec, (p: Path) => p)
      def apply(pathSpec: Traversable[Path]) = new MappedPathSpecBuilder(pathSpec.asInstanceOf[PathSpec], (p: Path) => p)
      //only one intended for use
      def apply(pathSpec: PathSpec, f: Path=>Path) = new MappedPathSpecBuilder(pathSpec, f)
    }

}

abstract class PathSpec extends Traversable[Path] with TraversableLike[Path, PathSpec] {

  def +++(includes: PathSpec): PathSpec = {
    (this, includes) match {
      case (xThis: SimplePathSpec, xIncludes: SimplePathSpec) => {
        new SimplePathSpec((xThis.root ++ xIncludes.root): _*)
      }
      case (xThis: CompoundPathSpec, xIncludes: CompoundPathSpec) => {
        new CompoundPathSpec((xThis.pathSpecSeq ++ xIncludes.pathSpecSeq): _*)
      }
      case (xThis: PathSpec, xIncludes: CompoundPathSpec) => {
        new CompoundPathSpec((Seq(xThis) ++ xIncludes.pathSpecSeq): _*)
      }
      case (xThis: CompoundPathSpec, xIncludes: PathSpec) => {
        new CompoundPathSpec((xThis.pathSpecSeq ++ Seq(xIncludes)): _*)
      }
      case (xThis: PathSpec, xIncludes: PathSpec) => {
        new CompoundPathSpec(Seq(xThis) ++ Seq(xIncludes): _*)
      }
    }
  }

  def ---(excludes: PathSpec): PathSpec = new ExclusionPathSpec(this, excludes)

  def *(matcher: PathMatcher): PathSpec = new TreeWalkPathSpec(this, 1, matcher)

  def **(matcher: PathMatcher): PathSpec = new TreeWalkPathSpec(this, Int.MaxValue, matcher)

  def **(matcher: PathMatcher, d: Int): PathSpec = new TreeWalkPathSpec(this, d, matcher)

  def *** : PathSpec = this **(PathMatcher( """.*""".r), Int.MaxValue)

  def /(literal: String): PathSpec = this **(PathMatcher(literal), 1)

  def ancestorsOf(p: Path): Set[Path]

  protected def underlying: PathSpec = this

  override def newBuilder: Builder[Path, PathSpec] = new Builder[Path, PathSpec] {
    val paths = new ArrayBuffer[Path]
    def apply() = this
    def +=(p: Path) = {paths.append(p); this}
    def result: PathSpec = new SimplePathSpec(paths: _*)
    def clear() = paths.clear()
  }

  override def map[B, That](f: Path => B)(implicit bf: CanBuildFrom[PathSpec, B, That]): That = {
    bf match {
      case psbf: PathSpecCanBuildFrom => psbf(this, f.asInstanceOf[Path => Path]).result().asInstanceOf[That]
      case _ => super.map(f)
    }
  }

  override def filter(p: Path => Boolean): PathSpec = {
    new FilteredPathSpec(this, p)
  }

}

final class SimplePathSpec(roots: Path*) extends PathSpec {
  val root: Seq[Path] = roots
  override def foreach[U](f: Path => U) = {
    roots.foreach((p: Path) => f(p))
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

final private class CompoundPathSpec(pathSpecs: PathSpec*) extends PathSpec {

  val pathSpecSeq: Seq[PathSpec] = pathSpecs

  override def foreach[U](f: Path => U) = {
    for (i <- pathSpecSeq) i.foreach(f)
  }

  override def ancestorsOf(p: Path): Set[Path] = {
    var ancestorSet = Set[Path]()
    for (pathSpec <- pathSpecSeq) {
      ancestorSet = ancestorSet ++ pathSpec.ancestorsOf(p)
    }
    ancestorSet
  }
}

final private class ExclusionPathSpec(superset: PathSpec, excluded: PathSpec) extends PathSpec {

  override def foreach[U](f: Path => U) = {
    superset.foreach((p: Path) => if (!excluded.ancestorsOf(p).contains(p)) f(p))
  }

  override def ancestorsOf(i: Path): Set[Path] = {
    val aAnc: Set[Path] = superset.ancestorsOf(i)
    val bAnc: Set[Path] = excluded.ancestorsOf(i)
    aAnc -- bAnc
  }
}

final private class FilteredPathSpec(p: PathSpec, func: Path => Boolean) extends PathSpec {
  override def foreach[U](f: Path => U) = {
    p.foreach((p: Path) => if(func(p)) f(p))
  }

  override def ancestorsOf(i: Path): Set[Path] = {
    p.ancestorsOf(i)
  }
}

final private class TreeWalkPathSpec(memberPathSpec: PathSpec, depth: Int, matcher: PathMatcher) extends PathSpec {

  override def foreach[U](f: Path => U) = {
    var d: Int = depth
    for (root <- memberPathSpec) {
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
    val ancestorRoots = memberPathSpec.ancestorsOf(p)
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

final class MappedPathSpec(pathSpec: PathSpec, func: Path => Path) extends PathSpec {

  override def foreach[U](f: Path => U) = {
    for (p <- pathSpec)
      f(func(p))
  }
  override def ancestorsOf(p: Path): Set[Path] = {
    var ancestorSet = Set[Path]()
    for (candidateAncestor <- pathSpec)
      if (p startsWith func(candidateAncestor))
        ancestorSet += func(candidateAncestor)
    ancestorSet
  }

}
