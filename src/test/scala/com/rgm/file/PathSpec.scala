package com.rgm.file

import org.scalacheck._
import org.scalatest._

/**
 * Created by sshivaprasad on 6/4/14.
 */
object PathSpec extends Properties("Path")
{
  import Prop._

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

}
