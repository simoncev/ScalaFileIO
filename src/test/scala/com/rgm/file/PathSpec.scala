package com.rgm.file
import org.scalacheck._

import org.scalatest._

/**
 * Created by sshivaprasad on 6/4/14.
 */
object PathSpec extends Properties("Path")
{
  import Prop._
  import Generators._

  /*
  //properties for path function
  property("path") = forAll{ (p: Path) =>  p.path == p.jpath}

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

  //properties for parent
  property
*/

  // sibling, both string and path
  property("Sibling has same parent (called with Path)") =  forAll {(p: Path, q: Path) => p.sibling(q).parent == p.parent }

  property("Siblings are both absolute/both relative (called with Path)") = Path("/foo/bar").isAbsolute == Path("/foo/bar").sibling(Path("foo")).isAbsolute

  property("Sibling has same parent (called with String)") =  Path("/foo/bar").sibling(Path("foo")).parent == Path("/foo/bar").parent

  property("Siblings are both absolute/both relative (called with String)") = Path("/foo/bar").isAbsolute == Path("/foo/bar").sibling(Path("foo")).isAbsolute

  // resolve, both string and path

  property("Resolved path at least as long as each original paths") = 1 ==2

  property("Resolved path shorter than or equal to path1.segmentCount + path2.segmentCount") = 1 == 2

  property("Resolved path is absolute iff path1 is absolute") = 1 == 2

  property("Resolved path's name is prefixed path1's name") = 1 == 2

  // "/", both string and path

  property("/ path at least as long as each original paths") = 1 ==2

  property("/ path shorter than or equal to path1.segmentCount + path2.segmentCount") = 1 == 2

  property("/ path is absolute iff path1 is absolute") = 1 == 2

  property("/ path's name is prefixed path1's name") = 1 == 2

  // relativeTo, both string and path
  property("relativeTo is never absolute") = 1 == 2

  property("relativeTo test for absolute/relative, relative/absolute") = 1 == 2

  property("relativeTo empty when you are relativeTo yourself") = 1 == 2

  // relativize, both string and path
  property("relativize is never absolute") = 1 == 2

  property("relativize test for absolute/relative, relative/absolute") = 1 == 2

  property("relativize empty when you are relativeTo yourself") = 1 == 2

  // to URI and URL

  property("toUri ends with the path's name") = 1 == 2

  property("toUrl ends with the path's name") = 1 == 2

  //jfile

  //normalize

  property("Normalized path does not contain a .. or a .") = 1 == 2

  property("Normalized path is no longer than the original") = 1 == 2

  property("Normalization is impossible between relative and absolute paths") = 1 == 2

  // isAbsolute

  property("First element is root iff isAbsolute") = 1 == 2

  //toAbsolute

  property("toAbsolute is at least as long as original path") = 1 == 2

  property("toAbsolute makes a path isAbsolute") = 1 == 2

  // endsWith, both string and path

  property("") = 1 == 2

  // startsWith, both string and path

  property("All absolute paths start with root") = 1 == 2

  //

  property("") = 1 == 2




}
