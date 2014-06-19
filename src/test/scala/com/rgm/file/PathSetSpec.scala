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
    FileSystem.default.pathMatcher("glob:*.tmp")
    Path(srcGlobal).createTempFile("foo", "tmp")
    Path(srcGlobal).createTempFile("bar", "tmp")
    Path(srcGlobal).createTempFile("baz", "scala")

  }

}