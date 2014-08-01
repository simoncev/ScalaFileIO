package com.rgm.file

import java.nio.file.{PathMatcher => JPathMatcher, FileSystems}
import scala.language.implicitConversions
import scala.util.matching.Regex

object PathMatcher {
  implicit def globMatcher(s: String): PathMatcher = apply(s)
  implicit def regexMatcher(r: Regex): PathMatcher = apply(r)
  
  implicit def fromJava(matcher: JPathMatcher): PathMatcher = new FunctionPathMatcher((p: Path) => matcher.matches(p.jpath))

  def apply(matcher: Path => Boolean): PathMatcher = new FunctionPathMatcher(matcher)
  def apply(s: String): PathMatcher = fromJava(FileSystems.getDefault.getPathMatcher("glob:" + s))
  def apply(r: Regex): PathMatcher = fromJava(FileSystems.getDefault.getPathMatcher("regex:" + r))

  object All extends PathMatcher {
    def matches(path: Path): Boolean = true
  }


}

trait PathMatcher {
  def matches(path: Path): Boolean

  /** Returns a matcher that accepts a `Path` if it matches either `this` matcher or `that` matcher. */
 	def || (that: PathMatcher): PathMatcher = PathMatcher(p => this.matches(p) || that.matches(p))
 
  /** Returns a matcher that accepts a `Path` if it matches `this` matcher and `that` matcher. */
 	def && (that: PathMatcher): PathMatcher = PathMatcher(p => this.matches(p) && that.matches(p))
 
  /** Returns a matcher that accepts a `Path` if it matches either `this` matcher but not `that` matcher. */
 	def -- (that: PathMatcher): PathMatcher = PathMatcher(p => this.matches(p) && !that.matches(p))
 
 	/** Returns a matcher that accepts a `Path` if it does not match `this` matcher. */
 	def unary_! : PathMatcher = PathMatcher(!this.matches(_))
}


class FunctionPathMatcher(matcher: Path => Boolean) extends PathMatcher {
  def matches(path: Path): Boolean = matcher(path)
}

object RegexPathMatcher {
  def apply(regex: Regex) = new RegexPathMatcher(regex)
}

class RegexPathMatcher(regex: Regex) extends PathMatcher {
  val matcher = PathMatcher.fromJava(FileSystems.getDefault.getPathMatcher("regex:" + regex))

  def matches(path: Path): Boolean = matcher.matches(path)

}

object RegexNameMatcher {
  def apply(regex: Regex) = new RegexNameMatcher(regex)

}

class RegexNameMatcher(regex: Regex) extends PathMatcher {
  val matcher = PathMatcher.fromJava(FileSystems.getDefault.getPathMatcher("regex:" + regex))

  def matches(path: Path): Boolean = matcher.matches(path.name)

}

object GlobPathMatcher {
  def apply(glob: String) = new GlobPathMatcher(glob)
}

class GlobPathMatcher(glob: String) extends PathMatcher {

  val matcher = PathMatcher.fromJava(FileSystems.getDefault.getPathMatcher("glob:" + glob))

  def matches(path: Path): Boolean = matcher.matches(path)
}

object GlobNameMatcher {
  def apply(glob: String) = new GlobNameMatcher(glob)
}

class GlobNameMatcher(glob: String) extends PathMatcher {

  val matcher = PathMatcher.fromJava(FileSystems.getDefault.getPathMatcher("glob:" + glob))

  def matches(path: Path): Boolean = matcher.matches(path.name)

}