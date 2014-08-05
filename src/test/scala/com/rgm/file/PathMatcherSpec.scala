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

  property("GlobPathMatcher only matches * with paths of one segment") =
    forAll{(p: Path) => p.isAbsolute || (GlobPathMatcher("*").matches(p) == (p.segmentCount == 1))}

  property("Can use glob to find files with extensions") =
    forAll(genLegalString){(s: String) => s == ".." || "*?.*".matches(Path(s)) == (Path(s).extension != None)}

  property("Can use regex to capture files of a given depth") =
    forAll(Gen.chooseNum(1,8), genPath) {(i: Int, p: Path) =>
      if (p.isAbsolute)
        if (i < 2) true else!(RegexPathMatcher(("/" + ("[^/]*/" * (i - 2)) + "[^/]+").r).matches(p) ^ p.segmentCount == i)
      else
        !(RegexPathMatcher((("[^/]*/" * (i - 1)) + "[^/]*").r).matches(p) ^ p.segmentCount == i)
    }

  property("GlobNameMatcher matches * with any path") =
    forAll{(p: Path) => p.isAbsolute || (p.segmentCount == 0 ^ GlobNameMatcher("*").matches(p))}

  property("RegexNameMatcher matches on the name, not the entire path") =
    forAll(genPath, genLegalCharsString){(p: Path, s: String) => s.length == 0 || RegexNameMatcher(s.r).matches(p / Path(s))}



}
