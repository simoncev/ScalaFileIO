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

  private class FunctionPathMatcher(matcher: Path => Boolean) extends PathMatcher {
    def matches(path: Path): Boolean = matcher(path)
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



trait PathNameMatcher extends PathMatcher {
  def matches(name: String): Boolean
  final def matches(path: Path) = matches(path.name)
}
