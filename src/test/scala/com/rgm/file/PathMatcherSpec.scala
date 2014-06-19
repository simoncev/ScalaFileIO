package com.rgm.file

import org.scalacheck._
import PathMatcher.{regexMatcher, globMatcher}

/**
 * Created by thausler on 6/18/14.
 */
object PathMatcherSpec extends Properties("PathMatcher") {

  import Prop._
  import Generators._

  property("Implicit conversion from regex to PathMatcher") =
    forAll {(p: Path) => ".*".r.matches(p)}

  property("Implicit conversion from string to glob") =
    forAll{(p: Path) => p.isAbsolute || ("*".matches(p) == (p.segmentCount == 1))}


}
