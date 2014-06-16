package com.rgm.file

import org.scalacheck._
import org.scalacheck.Gen._
import scala.util.Random
import scala.math._

/**
 * Created by thausler on 6/5/14.
 */
object Generators {
  import Arbitrary.arbitrary

  protected val legalChars = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ List('_', '-', '+', '.', ' ')).toSeq

  def sampleWithReplacement(seq: Seq[Char], n: Int): Seq[Char] = {
    if (n == 0)
      List[Char]()
    else
      Random.shuffle(seq).take(1) ++ sampleWithReplacement(seq, n-1)
  }

  //Generate strings for building paths
  val genLegalCharsString: Gen[String] = Gen.chooseNum(1,10).map(n => sampleWithReplacement(legalChars, Random.nextInt(n)).mkString(""))

//  val genLegalCharsString: Gen[String] = Gen.alphaStr

  val genParentDirString: Gen[String] = Gen.const("..")

  val genCurrentDirString: Gen[String] = Gen.const(".")

  val genLegalString : Gen[String] = frequency((10, genParentDirString),
                                               (5, genCurrentDirString),
                                               (85, genLegalCharsString))

  //Use lists of strings to build path names
  val genPathStringList: Gen[List[String]] = Gen.listOf(genLegalString)

  val genRelativePathString: Gen[String] = genPathStringList.map(str => str.mkString("/")).filter(str => !str.startsWith("/"))

  val genAbsolutePathString: Gen[String] = genRelativePathString.map(str => "/".concat(str.mkString("/")))

  val genPathStrings: Gen[String] = frequency((50, genRelativePathString),
                                              (50, genAbsolutePathString))

  val genPathStringWithExtension: Gen[String] = genPathStrings.map(str => str + "." + genLegalCharsString.sample.get)

  //use path names to build Paths
  val genRelativePath: Gen[Path] = genRelativePathString.map(str => Path(str))

  val genAbsolutePath: Gen[Path] = genAbsolutePathString.map(str => Path("/".concat(str)))

  val genPath: Gen[Path] = frequency((50, genRelativePath),
                                     (50, genAbsolutePath))


  implicit val arbPath: Arbitrary[Path] = Arbitrary(genPath)

  implicit val arbString: Arbitrary[String] = Arbitrary(genPathStrings)



}
