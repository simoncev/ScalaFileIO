package com.rgm.file

import org.scalacheck._
import org.scalacheck.Gen._

/**
 * Created by thausler on 6/5/14.
 */
object Generators {
  import Arbitrary.arbitrary


  val genLegalString: Gen[String] = Gen.alphaStr

  val genPathStrings: Gen[List[String]] = Gen.listOf(genLegalString)

  val genPath: Gen[Path] = genPathStrings.map(str => Path(str.mkString("/")))

  implicit val arbPath: Arbitrary[Path] = Arbitrary(genPath)

  //  val genFileDelimiter: Gen[String] = "/"


}
