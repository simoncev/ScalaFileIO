package com.rgm.file

import java.io.IOException
import java.nio.file.attribute._
import java.nio.file.{Path => JPath, _}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.collection.{GenTraversableOnce, TraversableLike}

object PathSpec {
  def apply(paths: Path*): PathSpec = new SimplePathSpec(paths: _*)

  implicit def canBuildFrom: CanBuildFrom[Traversable[Path], Path, PathSpec] = new PathSpecCanBuildFrom

  /**Defines the builder to be used by Traversable[Path] when creating literal PathSpecs*/
  def newBuilder: Builder[Path, PathSpec] = new Builder[Path, PathSpec] {
    private[this] val paths = new ArrayBuffer[Path]
    def +=(p: Path) = {paths.append(p); this}
    def result(): PathSpec = new SimplePathSpec(paths: _*)
    def clear() = paths.clear()
  }

  private class PathSpecCanBuildFrom extends CanBuildFrom[Traversable[Path], Path, PathSpec] {
    def apply(): Builder[Path,PathSpec] = newBuilder
    def apply(pathSpec: Traversable[Path]) = {
      val acc = newBuilder
      for (p <- pathSpec)
        acc += p
      acc
    }
    def buildMappedSpec(pathSpec: PathSpec, f: Path=>Path) = new MappedPathSpec(pathSpec, f)
    def buildFlatMap(pathSpec: PathSpec, func: Path => GenTraversableOnce[Path]): PathSpec = new FlatMapPathSpec(pathSpec, func)
  }

}

abstract class PathSpec extends Traversable[Path] with TraversableLike[Path, PathSpec] {
  import PathSpec.PathSpecCanBuildFrom

  /**Returns the union of the two PathSpecs.  May contain duplicates*/
  def +++(includes: PathSpec): PathSpec = (this, includes) match {
    case (xThis: SimplePathSpec, xIncludes: SimplePathSpec) => new SimplePathSpec((xThis.roots ++ xIncludes.roots): _*)
    case (xThis: CompoundPathSpec, xIncludes: CompoundPathSpec) => new CompoundPathSpec((xThis.pathSpecs ++ xIncludes.pathSpecs): _*)
    case (xThis: PathSpec, xIncludes: CompoundPathSpec) => new CompoundPathSpec((Seq(xThis) ++ xIncludes.pathSpecs): _*)
    case (xThis: CompoundPathSpec, xIncludes: PathSpec) => new CompoundPathSpec((xThis.pathSpecs ++ Seq(xIncludes)): _*)
    case (xThis: PathSpec, xIncludes: PathSpec) => new CompoundPathSpec(Seq(xThis) ++ Seq(xIncludes): _*)
  }

  /**Returns a PathSpec with the members of excludes excluded from this path set*/
  def ---(excludes: PathSpec): PathSpec = new ExclusionPathSpec(this, excludes)

  /**Returns a PathSpec of the children who match the matcher*/
  def *(matcher: PathMatcher): PathSpec = children(matcher)

  /**Returns a PathSpec of the descendants to depth d who match the matcher*/
  def **(matcher: PathMatcher): PathSpec = descendants(matcher)

  /**Returns a PathSpec of all descendants of this*/
  def *** : PathSpec = descendants(PathMatcher.All)

  /**Returns a PathSpec of the children who match the glob literal*/
  def /(literal: String): PathSpec = descendants(PathMatcher(literal), 1)

  /**Returns the prefixes of p which are members of this (can include p).*/
  private[file] def ancestorsOf(p: Path): Set[Path]

  /**Returns the children who match this matcher*/
  def children(matcher: PathMatcher = PathMatcher.All): PathSpec = new TreeWalkPathSpec(this, 1, matcher)

  /**Returns the descendants up to depth d who match matcher*/
  def descendants(matcher: PathMatcher = PathMatcher.All, depth: Int = -1): PathSpec = {
    if (depth >= 0)
      new TreeWalkPathSpec(this, depth, matcher)
    else
      new TreeWalkPathSpec(this, Int.MaxValue, matcher)
  }

  /**Defines the builder to be used by Traversable[Path] when creating literal PathSpecs*/
  override def newBuilder: Builder[Path, PathSpec] = PathSpec.newBuilder

  /** Applies a filter to instance PathSet, it is lazy */
  override def filter(p: Path => Boolean): PathSpec = new FilteredPathSpec(this, p)

  /** Applies a filter to instance PathSet, it is lazy */
  override def withFilter(p: Path => Boolean): PathSpec = new FilteredPathSpec(this,p)

  /** Builds a new collection by applying a function to all elements */
  override def map[B, That](f: Path => B)(implicit bf: CanBuildFrom[PathSpec, B, That]): That = {
    bf match {
      case psbf: PathSpecCanBuildFrom => psbf.buildMappedSpec(this, f.asInstanceOf[Path => Path]).asInstanceOf[That]
      case _ => super.map(f)
    }
  }

  /** Builds a new collection by applying the argument function to all its elements that satify the predicate and concatenates the results */
  override def flatMap[B, That](f: Path => GenTraversableOnce[B])(implicit bf: CanBuildFrom[PathSpec, B, That]): That = {
    bf match {
      case psbf: PathSpecCanBuildFrom => psbf.buildFlatMap(this, f.asInstanceOf[Path => GenTraversableOnce[Path]]).asInstanceOf[That]
      case _ => super.flatMap(f)
    }
  }

  override def collect[B, That](pf: PartialFunction[Path, B])(implicit bf: CanBuildFrom[PathSpec, B, That]): That =  {
    filter(pf.isDefinedAt).map(pf)
  }
}

final private class SimplePathSpec(val roots: Path*) extends PathSpec {
  override def foreach[U](f: Path => U): Unit = roots.foreach(f)

  private[file] override def ancestorsOf(i: Path): Set[Path] = {
    var result: Set[Path] = Set[Path]()
    for(p <- roots) {
      if(i startsWith p)
        result += p
    }
    result
  }
}

final private class CompoundPathSpec(val pathSpecs: PathSpec*) extends PathSpec {
  override def foreach[U](f: Path => U) = {
    for (i <- pathSpecs) i.foreach(f)
  }

  private[file] override def ancestorsOf(p: Path): Set[Path] = {
    var ancestorSet = Set[Path]()
    for (pathSpec <- pathSpecs) {
      ancestorSet = ancestorSet ++ pathSpec.ancestorsOf(p)
    }
    ancestorSet
  }
}

final private class ExclusionPathSpec(superset: PathSpec, excluded: PathSpec) extends PathSpec {

  override def foreach[U](f: Path => U) = {
    superset.foreach((p: Path) => if (!excluded.ancestorsOf(p).contains(p)) f(p))
  }

  private[file] override def ancestorsOf(i: Path): Set[Path] = {
    val aAnc: Set[Path] = superset.ancestorsOf(i)
    val bAnc: Set[Path] = excluded.ancestorsOf(i)
    aAnc -- bAnc
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

  private[file] override def ancestorsOf(p: Path): Set[Path] = {
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

final private class FilteredPathSpec(p: PathSpec, func: Path => Boolean) extends PathSpec {
  override def foreach[U](f: Path => U): Unit = p.foreach((p: Path) => if(func(p)) f(p))
  private[file] override def ancestorsOf(i: Path): Set[Path] = p.ancestorsOf(i)
}

final private class MappedPathSpec(pathSpec: PathSpec, func: Path => Path) extends PathSpec {
  override def foreach[U](f: Path => U) = {
    for (p <- pathSpec)
      f(func(p))
  }
  private[file] override def ancestorsOf(p: Path): Set[Path] = {
    var ancestorSet = Set[Path]()
    for (candidateAncestor <- pathSpec)
      if (p startsWith func(candidateAncestor))
        ancestorSet += func(candidateAncestor)
    ancestorSet
  }

}

final private class FlatMapPathSpec(pathSpec: PathSpec, func: Path => GenTraversableOnce[Path]) extends PathSpec {
  override def foreach[U](f: Path => U) = {
    for(p <- pathSpec)
      for(q <- func(p))
        f(q)
  }

  private[file] override def ancestorsOf(p: Path): Set[Path] = {
    var ancestorSet = Set[Path]()
    for (candidateAncestor <- pathSpec)
      if (p startsWith Path(func(Path(candidateAncestor.toString)).toString))
        ancestorSet += Path(func(Path(candidateAncestor.toString)).toString)
    ancestorSet
  }
}
