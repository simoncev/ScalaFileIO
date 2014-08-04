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

  property("FullPathMatcher only matches * with paths of one segment") =
    forAll{(p: Path) => p.isAbsolute || (GlobPathMatcher("*").matches(p) == (p.segmentCount == 1))}

  property("Can use glob to find files with extensions") =
    forAll(genLegalString){(s: String) => s == ".." || "*?.*".matches(Path(s)) == (Path(s).extension != None)}

  property("Can use regex to capture files of a given depth") =
    forAll(Gen.chooseNum(1,8), genPath) {(i: Int, p: Path) =>
      if (p.isAbsolute)
        if (i < 2) true else!(("/" + ("[^/]*/" * (i - 2)) + "[^/]+").r.matches(p) ^ p.segmentCount == i)
      else
        !((("[^/]*/" * (i - 1)) + "[^/]*").r.matches(p) ^ p.segmentCount == i)
    }
}
