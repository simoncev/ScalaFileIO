package com.rgm.file

import java.nio.file.{Path => JPath, _}
import scala.util.Random
import scala.collection.mutable.ListBuffer
/**
 * Created by sshivaprasad on 6/9/14.
 */
object FSTest {

  def main(args: Array[String])
  {
    val src = "/Users/sshivaprasad/Documents/TEST/src/"
    val target = "/Users/sshivaprasad/Documents/TEST/target/"
    val p = new Path(FileSystems.getDefault.getPath(src))
    val q = new Path(FileSystems.getDefault.getPath(target))
    if(p.exists)
      p.deleteRecursively
    p.createDirectory
    if(q.exists)
      q.deleteRecursively
    q.createDirectory

    val (dirs,fils) = createFS(p)

    //Copy
    println("Does copyTo work? " + testCopyTo(fils, target).toString)
    q.deleteRecursively
    q.createDirectory

    println("Does moveFile work? " + testMoveFile(fils, target).toString)
    q.deleteRecursively
    q.createDirectory

    println("Does moveDirectory work? " + testMoveDirectory(dirs, target).toString)


    //q.deleteRecursively
    //q.createDirectory

  }

  def createFS(p: Path) : (ListBuffer[JPath],ListBuffer[JPath]) =
  {
    val start = 0
    val end = 9
    val no = start + Random.nextInt(end - start + 1)
    var fls = new ListBuffer[JPath]()
    var dirs = new ListBuffer[JPath]()
    for(x <- 1 to no)
    {
      val tmp = p.createTempDir(x.toString + "_")
      dirs += tmp.jpath
      fls += tmp.createTempFile(x + "_", ".tmp").jpath
    }
    (dirs,fls)
  }

  def testCopyTo(fls: ListBuffer[JPath], target: String): Boolean =
  {
    for(i <- fls.toList)
    {
      val tmp = new Path(i)
      try {
        tmp.copyTo(target + i.toString.split("/").last)
      }
      catch {
        case nsfe: NoSuchFileException => return false
      }
    }
    //val flag = false
    for(x <- fls.toList)
    {
      val tmp = new Path(FileSystems.getDefault.getPath(target + x.toString.split("/").last))
      if(tmp.nonExistent && !tmp.isFile && (tmp.size.get != 0))
      {
        println("DID NOT COPY CORRECTLY - " + target + x.toString.split("/").last)
        false
      }
    }
    true
  }

  def testMoveFile(fls: ListBuffer[JPath], target: String): Boolean =
  {
    for(i <- fls.toList)
    {
      val tmp = new Path(i)
      try {
        tmp.moveFile(target + i.toString.split("/").last)
      }
      catch {
        case nsfe: NoSuchFileException => return false
      }
    }
    //val flag = false
    for(x <- fls.toList)
    {
      val tmp = new Path(FileSystems.getDefault.getPath(target + x.toString.split("/").last))
      val tmp2 = new Path(x)
      if(tmp.nonExistent && !tmp.isFile && tmp2.exists && (tmp.size.get != 0))
      {
        println("DID NOT COPY CORRECTLY - " + target + x.toString.split("/").last)
        false
      }
    }
    true
  }

  def testMoveDirectory(dirs: ListBuffer[JPath], target: String): Boolean =
  {
    for(i <- dirs.toList)
    {
      val tmp = new Path(i)
      try {
        tmp.moveDirectory(target + i.toString.split("/").last)
      }
      catch {
        case nsfe: NoSuchFileException => return false
      }
    }
    //val flag = false
    for(x <- dirs.toList)
    {
      val tmp = new Path(FileSystems.getDefault.getPath(target + x.toString.split("/").last))
      if(tmp.nonExistent && !tmp.isDirectory)
      {
        println("DID NOT COPY CORRECTLY - " + target + x.toString.split("/").last)
        false
      }
    }
    true
  }
}



