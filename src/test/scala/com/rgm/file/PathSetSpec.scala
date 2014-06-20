package com.rgm.file

import java.nio.file.{Path =>JPath, _}
import org.scalatest.{FlatSpec, Suite, BeforeAndAfterEach}
import scala.collection.mutable.ListBuffer
import scala.util.{Try, Random}
import java.net.URI
import java.util


class PathSetSpec extends FlatSpec with FileSetupTeardown {

  behavior of "PathSet"

  override def createFS(p: Path) : (Array[JPath],Array[JPath]) =
  {
    var fls = new Array[JPath](0)
    var dirs = new Array[JPath](0)
    (dirs,fls)
  }

  it should "1. PathSet should find the current state of the file system" in {
    val matcher = PathMatcher(""".*""".r)
    val pathSet = PathSet(Path(srcGlobal), matcher, 3)
    val foo = Path(srcGlobal).createTempFile("foo", ".tmp")
    Path(srcGlobal).createTempFile("bar", ".tmp")
    Path(srcGlobal).createTempFile("baz", ".scala")
    var numTmps = 0
    pathSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 4)
    foo.delete()
    numTmps = 0
    pathSet.foreach(p => numTmps += 1)

    assert(numTmps == 3)
  }

  it should "3. PathSet should apply its filter to the elements it finds" in {
    val matcher = PathMatcher("*.tmp")
    val pathSet = PathSet(Path(srcGlobal), matcher, 3)
    val foo = Path(srcGlobal).createTempFile("foo", ".tmp")
    Path(srcGlobal).createTempFile("bar", ".tmp")
    Path(srcGlobal).createTempFile("baz", ".scala")
    var numTmps = 0
    pathSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 2)

  }

}