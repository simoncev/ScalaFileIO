package com.rgm.file

import org.scalacheck._
import org.scalacheck.Gen._

/**
 * Created by thausler on 6/5/14.
 */
object Generators {
  import Arbitrary.arbitrary

  implicit val arbPath: Arbitrary[Path] = Arbitrary(genPath)

  val genPath: Gen[Path] = Gen.alphaStr.map(str => Path(str)) //arbitrary[String].map(str => Path(str))

//  val genPath2: Gen[Path] = genPathStrings.map(dirs => dirs.mkString("/"))

  val genPathStrings: Gen[Seq[String]] = Gen.someOf(genString, genString)

  val genString: Gen[String] = arbitrary[String]

//  val genFileDelimiter: Gen[String] = "/"


}
