package com.rgm.file

import org.scalacheck._
import org.scalacheck.Gen._

/**
 * Created by thausler on 6/5/14.
 */
object Generators {
  import Arbitrary.arbitrary

  //protected val legalChars = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ List('_', '-', '+', '.', ' ') toList

  val genLegalCharsString: Gen[String] = Gen.alphaStr

  val genParentDirString: Gen[String] = ".."

  val genCurrentDirString: Gen[String] = "."

  val genLegalString : Gen[String] = frequency((10, genParentDirString),
                                               (5, genCurrentDirString),
                                               (85, genLegalCharsString))

  val genPathStringLists: Gen[List[String]] = Gen.listOf(genLegalString)

  val genRelativePathStrings: Gen[String] = genPathStringLists.map(str => str.mkString("/"))

  val genAbsolutePathStrings: Gen[String] = genRelativePathStrings.map(str => "/".concat(str.mkString("/")))

  val genPathStrings: Gen[String] = frequency((50, genRelativePathStrings),
                                              (50, genAbsolutePathStrings))

  val genRelativePath: Gen[Path] = genRelativePathStrings.map(str => Path(str))

  val genAbsolutePath: Gen[Path] = genAbsolutePathStrings.map(str => Path("/".concat(str)))

  val genPath: Gen[Path] = frequency((50, genRelativePath),
                                     (50, genAbsolutePath))


  implicit val arbPath: Arbitrary[Path] = Arbitrary(genPath)

  implicit val arbString: Arbitrary[String] = Arbitrary(genPathStrings)



}
