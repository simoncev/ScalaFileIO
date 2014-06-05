package com.rgm.file

import org.scalacheck._
import org.scalacheck.Gen._

/**
 * Created by thausler on 6/5/14.
 */
object Generators {
  import Arbitrary.arbitrary

  implicit val arbPath: Arbitrary[Path] = Arbitrary(genPath)

  val genPath: Gen[Path] = arbitrary[String].map(str => Path(str))

}
