package com.rgm.file

import org.scalacheck._

/**
 * Created by thausler on 6/18/14.
 */
object PathMatcherSpec extends Properties("PathMatcher") {

  import Prop._
  import Generators._

  property("Path matches iff it contains the regex in its toString") =
    forAll {(p: Path) => PathMatcher.regexMatcher(".*".r).matches(p)}

}
