package com.rgm.file

import java.nio.file.{Path =>JPath, _}
import org.scalatest.{FlatSpec, Suite, BeforeAndAfterEach}
import scala.collection.mutable.ListBuffer
import scala.util.{Try, Random}
import java.net.URI
import java.util


class PathSetSpec extends FlatSpec with FileSetupTeardown {

  behavior of "PathSet"

  val allMatcher = PathMatcher(""".*""".r)

  override def createFS(p: Path) : (Array[JPath],Array[JPath]) =
  {
    var fls = new Array[JPath](0)
    var dirs = new Array[JPath](0)
    (dirs,fls)
  }
  def buildTmpFileTree = {
    val src = Path(srcGlobal)
    val dir1 = src.createTempDir("dir1_")
    val dir2 = src.createTempDir("dir2_")
    val dir3 = dir1.createTempDir("dir3_")
    val dir4 = dir2.createTempDir("dir4_")

    dir1.createTempFile("file_1_",".tmp")
    dir1.createTempFile("file_2_",".tmp")
    dir3.createTempFile("file_3_",".tmp")
    dir4.createTempFile("file_4_", ".tmp")
    src.createTempFile("file_5_",".tmp")
  }

  it should "1. PathSet should find the current state of the file system" in {
    val pathSet = PathSet(Path(srcGlobal)) * allMatcher
    val foo = Path(srcGlobal).createTempFile("foo", ".tmp")
    Path(srcGlobal).createTempFile("bar", ".tmp")
    Path(srcGlobal).createTempFile("baz", ".scala")
    var numTmps = 0
    pathSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 3)
    foo.delete()
    numTmps = 0
    pathSet.foreach(p => numTmps += 1)
    assert(numTmps == 2)
    flagGlobal = true
  }


  it should "2. test should search at exactly the given depth" in {
    //building testing tree
    buildTmpFileTree
    var numTmps = 0
    (PathSet(Path(srcGlobal)) * allMatcher).foreach((p:Path) => numTmps+=1)
    assert(numTmps==3)
    numTmps = 0
    (PathSet(Path(srcGlobal)) * allMatcher * allMatcher).foreach((p:Path) => numTmps+=1)
    assert(numTmps==5)
    flagGlobal = true
  }

  it should "3. PathSet should apply its filter to the elements it finds" in {
    val matcher = PathMatcher(".*.tmp".r)
    val pathSet = PathSet(Path(srcGlobal)) * matcher
    Path(srcGlobal).createTempFile("foo", ".tmp")
    Path(srcGlobal).createTempFile("bar", ".tmp")
    Path(srcGlobal).createTempFile("baz", ".scala")
    var numTmps = 0
    pathSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 2)
    flagGlobal = true
  }

  it should "4. test *" in {
    buildTmpFileTree
    val pathSet = PathSet(Path(srcGlobal))
    var num = 0
    (pathSet * """.*""".r).foreach((p: Path) => num+=1)
    assert(num==3)
    flagGlobal = true
  }

  it should "5. Does not match root on searches of children" in {
    val pathSet = PathSet(Path(srcGlobal).createTempDir("file_1_")) * allMatcher
    var numFound = 0
    pathSet.foreach((p: Path) => numFound += 1)
    assert(numFound == 0)
    flagGlobal = true
  }

  it should "6. Apply filters built of globs to the elements it finds" in {
    val matcher = PathMatcher(srcGlobal + "*.tmp")
    val pathSet = PathSet(Path(srcGlobal)) * matcher
    Path(srcGlobal).createTempFile("foo", ".tmp")
    Path(srcGlobal).createTempFile("bar", ".tmp")
    Path(srcGlobal).createTempFile("baz", ".scala")
    val dir1 = Path(srcGlobal).createTempDir("dir1")
    dir1.createTempFile("foo", ".tmp")
    dir1.createTempFile("bar", ".tmp")
    dir1.createTempFile("baz", ".tmp")
    var numTmps = 0
    pathSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 2)

    numTmps = 0
    val pathSetAllDepths = PathSet(Path(srcGlobal)) ** matcher
    pathSetAllDepths.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 2)

    flagGlobal = true

  }

  
}