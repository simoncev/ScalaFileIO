package com.rgm.file

import org.scalatest.{Suite, BeforeAndAfterEach}
import java.nio.file.{FileSystems, Path => JPath}
import scala.util.Random

/**
 * Created by thausler on 6/19/14.
 */

trait FileSetupTeardown extends BeforeAndAfterEach { this: Suite =>

  var flagGlobal: Boolean = false
  var testNo: Int = 1
  var (srcGlobal: String, targetGlobal: String, dirsGlobal: Array[JPath] , filsGlobal: Array[JPath]) = ("","",new Array[JPath](10),new Array[JPath](10))

  def setup : (String, String, Array[JPath], Array[JPath]) =
  {
    val src = java.nio.file.Files.createTempDirectory("source_" + testNo + "_").toString + "/"
    val target =  java.nio.file.Files.createTempDirectory("target_" + testNo + "_").toString + "/"
    if(testNo == 1)
      println("File trees can be found in: " + src.split("/").init.mkString("/") + "/" + "\nStructure of folder <source/target>_<test number>_ if test fails")
    testNo += 1
    val sourcePath = new Path(FileSystems.getDefault.getPath(src))
    val targetPath = new Path(FileSystems.getDefault.getPath(target))
    if(sourcePath.exists())
      sourcePath.deleteRecursively()
    sourcePath.createDirectory()
    if(targetPath.exists())
      targetPath.deleteRecursively()
    targetPath.createDirectory()
    val (dirs,fils) = createFS(sourcePath)
    (src, target, dirs, fils)
  }

  def createFS(p: Path) : (Array[JPath],Array[JPath]) =
  {
    val start = 1
    val end = 9
    val no = start + Random.nextInt(end - start + 1)
    var fls = new Array[JPath](no)
    var dirs = new Array[JPath](no)
    for(x <- 0 to no-1)
    {
      val tmp = Path.createTempDirectory(p, x.toString + "_")
      dirs(x) = tmp.jpath
      fls(x) = Path.createTempFile(tmp, x + "_", ".tmp").jpath
    }
    (dirs,fls)
  }


  override def afterEach() = {
    try super.afterEach()
    finally{
      if(flagGlobal) {
        val p = new Path(FileSystems.getDefault.getPath(srcGlobal))
        val q = new Path(FileSystems.getDefault.getPath(targetGlobal))
        p.deleteRecursively()
        q.deleteRecursively()
      }
    }
  }

  override def beforeEach() = {
    try super.beforeEach()
    finally{
      flagGlobal = false
      val (src, target, dirs, fils) = setup
      srcGlobal = src
      targetGlobal = target
      dirsGlobal = dirs
      filsGlobal = fils
    }
  }

}
