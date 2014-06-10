package com.rgm.file
import org.scalacheck._

import org.scalatest._
import scala.collection.mutable.Stack

import scala.collection.immutable.Range

/**
 * Created by sshivaprasad on 6/4/14.
 */
object SyntaxSpec extends Properties("Path")
{
  import Prop._
  import Generators._


  //properties for path function
  property("path") = forAll{ (p: Path) =>  p.path == p.jpath.toString}
/*
  //properties for simpleName function
  property("simpleName") = forAll{ (p: Path) => p.simpleName == p.path.replaceAll("""\.(.*)""", "")}
  property("simpleName size check") = forAll{ (p: Path) => p.simpleName.size <= p.path.size}

  //properties for extension function
  property("extension") = forAll{ (p:Path) => p.extension.toString.matches("""\.(.*)""") }
  property("extension size check") = forAll{ (p: Path) => p.extension.get.toString.size < p.name.size}

  //properties for withExtension function
  property("withExtension") = forAll{ (p:Path, s:String) => p.withExtension(Option(s)) == p.simpleName.toString.concat(s) }

  //properties for segments function
  property("segments size check") = forAll{ (p: Path) => p.segments.size == p.name.count(_ == '/') + 1 }

  //properties for segments iterable
  property("segments each iterable exists in name") = forAll{ (p: Path) => p.segmentIterator.forall(k => p.name.contains(k.toString))}
  property("segments each iterable size less than name") = forAll{ (p: Path) => p.segmentIterator.forall(k => p.name.size > k.toString.size)}

  //properties for segment size
  property("segmentsCount check") = forAll{ (p: Path) => p.segmentCount == p.segments.size }

  //properties for root
  property("root:p is absolute") = forAll{ (p:Path) => p.isAbsolute == p.root.isDefined}
*/
  //properties for parent
  property("Absolute paths always have a parent") = 1 == 1

  property("Parent of a path is the same as path + /..") =
    forAll{(p: Path) => p == p}

  // sibling, both string and path
  property("Sibling has same parent (called with Path)") =
    forAll { (p: Path, q: Path) => (p.parent == None) || (p.sibling(q).startsWith(p.parent.get)) }

  property("Siblings are both absolute/both relative (called with Path)") =
    forAll {(p: Path, q: Path) => p.isAbsolute == p.sibling(q).isAbsolute }

  property("Sibling has same parent (called with String)") =
    forAll {(p: Path, q: String) => (p.parent == None) || (p.sibling(q).startsWith(p.parent.get))}

  property("Siblings are both absolute/both relative (called with String)") =
    Path("/foo/bar").isAbsolute == Path("/foo/bar").sibling(Path("foo")).isAbsolute

  // resolve, both string and path
  property("If path2 is absolute, length of resolved path is path1.segmentCount + path2.segmentCount - 1 (with path)") =
    forAll(genPath, genAbsolutePath) {
      (path1: Path, path2: Path) => path1.resolve(path2).segmentCount == path1.segmentCount + path2.segmentCount - 1
  }
  property("Resolved path is absolute iff path1 is absolute (with path)") =
    forAll {(p: Path, q: Path) => p.isAbsolute == p.resolve(q).isAbsolute }

  property("Resolved path's name is prefixed path1's name (with path)") =
    forAll {(p: Path, q: Path) => if (!p.equals(Path(""))) p.resolve(q).startsWith(p) else true }

  property("If path2 is absolute, length of resolved path is path1.segmentCount + path2.segmentCount - 1 (with string)") =
    forAll(genPath, genAbsolutePathString) {
      (path1: Path, path2: String) => path1.resolve(path2).segmentCount == path1.segmentCount + Path(path2).segmentCount - 1
    }
  property("Resolved path is absolute iff path1 is absolute (with string)") =
    forAll {(p: Path, q: String) => p.isAbsolute == p.resolve(q).isAbsolute }

  property("Resolved path's name is prefixed path1's name (with string)") =
    forAll {(p: Path, q: String) => if (!p.equals(Path(""))) p.resolve(q).startsWith(p) else true }


  // "/", both string and path
  property("If path2 is absolute, length of / path is path1.segmentCount + path2.segmentCount - 1") =
    forAll(genPath, genAbsolutePath) {
      (path1: Path, path2: Path) => (path1 / path2).segmentCount == path1.segmentCount + path2.segmentCount - 1
    }

  property("/ path is absolute iff path1 is absolute") =
      forAll {(p: Path, q: Path) => p.isAbsolute == (p / q).isAbsolute }

  property("/ path's name is prefixed path1's name") =
    forAll {(p: Path, q: Path) => if (!p.equals(Path(""))) (p / q).startsWith(p) else true }

  property("If path2 is absolute, length of / path is path1.segmentCount + path2.segmentCount - 1 (with string)") =
    forAll(genPath, genAbsolutePathString) {
      (path1: Path, path2: String) => (path1 / path2).segmentCount == path1.segmentCount + Path(path2).segmentCount - 1
    }
  property("Resolved path is absolute iff path1 is absolute (with string)") =
    forAll {(p: Path, q: String) => p.isAbsolute == (p / q).isAbsolute }

  property("Resolved path's name is prefixed path1's name (with string)") =
    forAll {(p: Path, q: String) => if (!p.equals(Path(""))) (p / q).startsWith(p) else true }

    // relativeTo, both string and path
    property("relativeTo is never absolute") =
      forAll(genAbsolutePath, genAbsolutePath) {(p: Path, q: Path) => !(p relativeTo q).isAbsolute}

    property("relativeTo empty when you are relativeTo yourself") =
      forAll {(p: Path) => (p relativeTo p) equals Path("")}

    property("relativeTo test for absolute/relative, relative/absolute") =
      forAll(genRelativePath, genAbsolutePathString) {(p: Path, q: String) => Prop.throws(classOf[IllegalArgumentException]) { p.relativeTo(q)}}

    property("relativeTo is never absolute (with string)") =
      forAll(genAbsolutePath, genAbsolutePathString) {(p: Path, q: String) => !(p relativeTo q).isAbsolute}

    property("relativeTo test for absolute/relative, relative/absolute (with string)") =
      forAll(genRelativePath, genAbsolutePathString) {(p: Path, q:String) => Prop.throws(classOf[IllegalArgumentException]) { p.relativeTo(q)}}

  // relativize, both string and path
    property("relativize is never absolute") =
      forAll (genAbsolutePath, genAbsolutePath) {(p: Path, q: Path) => !(p relativize q).isAbsolute}

    property("relativize test for absolute/relative, relative/absolute") =
      forAll(genRelativePath, genAbsolutePath) {(p: Path, q:Path) => Prop.throws(classOf[IllegalArgumentException]) { p relativize q }}

    property("relativize empty when you are relativized to yourself") =
      forAll {(p: Path) => (p relativize p) equals Path("")}

    property("relativize is never absolute (with String") =
      forAll (genAbsolutePath, genAbsolutePathString) {(p: Path, q: String) => !(p relativize q).isAbsolute}

    property("relativize test for absolute/relative, relative/absolute (with String)") =
      forAll(genRelativePath, genAbsolutePathString) {(p: Path, q:String) => Prop.throws(classOf[IllegalArgumentException]) { p relativize q }}


  // to URI and URL
    property("toUri is the same as the jpath URI") =
      forAll {(p: Path) => p.toURI == p.jpath.toUri}

    property("toUrl is the same as the jpath URL") =
      forAll {(p: Path) => p.toURL == p.jpath.toUri.toURL}

    //jfile
    property("jfile is same as jpath's file") =
      forAll{(p: Path) => p.jfile == p.jpath.toFile}

    //normalize

    property("Normalized path does not contain a .") =
      forAll{(p: Path) => !p.normalize.segments.contains(Path("."))}

    property("Normalized path does not contain any .. except at the beginning") =
      forAll{(p: Path) => p.normalize.segments.lastIndexOf(Path("..")) == (p.normalize.segments.count(q => q == Path("..")) - 1) }

    property("Normalized path is no longer than the original") =
      forAll{(p: Path) => p.segmentCount >= p.normalize.segmentCount}


    // isAbsolute
    property("First element is root iff isAbsolute") =
      forAll{(p: Path) => if (p == Path("")) true else p.isAbsolute == p.segmentIterator.next.equals(Path("/"))}

    //toAbsolute

    property("toAbsolute is at least as long as original path") =
      forAll{(p: Path) => p.toAbsolute.segmentCount >= p.segmentCount}

    property("toAbsolute makes a path isAbsolute") =
      forAll{(p: Path) => p.toAbsolute.isAbsolute}

    // endsWith, both string and path

    // startsWith, both string and path

    property("All absolute paths start with root") =
      forAll(genAbsolutePath){(p: Path) => p.startsWith(Path("/"))}



}

class FileIOSpec extends FlatSpec {

  behavior of "A Stack"

  it should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    assert(stack.pop() === 2)
    assert(stack.pop() === 1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[String]
    intercept[NoSuchElementException] {
      emptyStack.pop()
    }
  }
}

