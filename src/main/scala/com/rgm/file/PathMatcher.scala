package com.rgm.file

import java.nio.file.{PathMatcher => JPathMatcher, Path => JPath}
import scala.language.implicitConversions
import scala.util.matching.Regex

object PathMatcher {
  implicit def globMatcher(s: String): PathMatcher = ???
  implicit def regexMatcher(r: Regex): PathMatcher = ???
  
  implicit def fromJava(matcher: JPathMatcher) = new FunctionPathMatcher((p: Path) => matcher.matches(p.jpath))

  def apply(matcher: Path => Boolean): PathMatcher = new FunctionPathMatcher(matcher)
}

trait PathMatcher {
  def matches(path: Path): Boolean

  /** Returns a matcher that accepts a `Path` if it matches either `this` matcher or `that` matcher. */
 	def || (that: PathMatcher): PathMatcher = PathMatcher(p => this.matches(p) || that.matches(p))
 
  /** Returns a matcher that accepts a `Path` if it matches either `this` matcher and `that` matcher. */
 	def && (matcher: PathMatcher): PathMatcher = ???
 
  /** Returns a matcher that accepts a `Path` if it matches either `this` matcher but not `that` matcher. */
 	def -- (matcher: PathMatcher): PathMatcher = ???
 
 	/** Returns a matcher that accepts a `Path` if it does not match `this` matcher. */
 	def unary_! : PathMatcher = ???
}

class FunctionPathMatcher(matcher: Path => Boolean) extends PathMatcher {
  def matches(path: Path): Boolean = matcher(path)
}

trait PathNameMatcher extends PathMatcher {
  def matches(name: String): Boolean
  final def matches(path: Path) = matches(path.name)
}
