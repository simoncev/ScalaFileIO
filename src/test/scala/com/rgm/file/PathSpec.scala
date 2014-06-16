package com.rgm.file

import org.scalacheck._
import org.scalatest._

import java.nio.file.{Path => JPath, _}
import scala.collection.mutable._


import scala.util._


/**
 * Created by sshivaprasad on 6/4/14.
 */
object SyntaxSpec extends Properties("Path")
{
  import Prop._
  import Generators._


  //properties for path function
  property("path agrees with jpath") =
    forAll{ (p: Path) =>  p.path == p.jpath.toString}

  //properties for simpleName function
  property("simpleName and extension combine to name") =
    forAll{ (p: Path) => p.name == (if (p.extension == None) p.simpleName else p.simpleName + "." + p.extension.get)}

  property("simpleName size check") =
    forAll{ (p: Path) => p.simpleName.size <= p.path.size}

  //properties for extension function
  property("extension size check") =
    forAll{ (p: Path) => if (p.extension == None) true else p.extension.get.toString.size < p.name.size}

  property("extension has no periods") =
    forAll{ (p: Path) => if (p.extension == None) true else !p.extension.get.contains('.')}

  property("No extension for hidden files with no other dots") =
    forAll(genPath, Gen.alphaStr) { (p: Path, s: String) => if (s == "") true else p.resolve(s).extension == None}

  //properties for withExtension function
  property("withExtension'ed strings always have extensions unless path/extension are empty") =
    forAll(genPath, Gen.alphaStr){ (p: Path, s:String) =>
      if (p == Path("") || p == Path("/") || s == "")
        true
      else
        p.withExtension(Some(s)).extension != None}

  //properties for segments function
  property("segments size check") =
    forAll{ (p: Path) => p.segments.size <= p.path.count(_ == '/') + 1 }

  //properties for segments iterable
  property("segments each item returned by iterator exists in name") =
    forAll{ (p: Path) =>
      var allFound = true
      for (seg <- p.segmentIterator)
        allFound = allFound && p.path.contains(seg.path)
      allFound
    }

  //properties for root
  property("root:p is absolute") =
    forAll{ (p:Path) => p.isAbsolute == p.root.isDefined}

  //properties for parent
  property("Parent of a normalized path is the same as path + /..") =
    forAll{(p: Path) =>
      if (p.normalize.parent != None && p.normalize.segments.last != Path(".."))
        p.normalize.parent.get == p.normalize.resolve("..").normalize
      else
        true
    }

  property("Parent is prefix of its path") =
    forAll{(p: Path) => if (p.parent != None) p.startsWith(p.parent.get) else true}

  // sibling, both string and path
  property("Sibling has same parent (called with Path)") =
    forAll { (p: Path, q: Path) => (p.parent == None || p.sibling(q).startsWith(p.parent.get)) }

  property("Siblings are both absolute/both relative (called with Path)") =
    forAll {(p: Path, q: Path) => if (p.root != None && p == p.root.get) true else p.isAbsolute == p.sibling(q).isAbsolute }

  property("Sibling has same parent (called with String)") =
    forAll {(p: Path, q: String) => (p.parent == None || p.sibling(q).startsWith(p.parent.get))}

  property("Siblings are both absolute/both relative (called with String)") =
    forAll { (p: Path, s: String) => if (p.root != None && p == p.root.get) true else p.isAbsolute == p.sibling(Path(s)).isAbsolute}

  // resolve, both string and path
  property("If path2 is absolute, length of resolved path is path1.segmentCount + path2.segmentCount - 1 (with path)") =
    forAll(genPath, genAbsolutePath) {
      (path1: Path, path2: Path) =>
        if  (path1.path == "" && path2.path != "/")
          path1.resolve(path2).segmentCount == path1.segmentCount + path2.segmentCount - 2
        else
          path1.resolve(path2).segmentCount == path1.segmentCount + path2.segmentCount - 1
    }
  property("Resolved path is absolute iff path1 is absolute (with path)") =
    forAll {(p: Path, q: Path) => p.isAbsolute == p.resolve(q).isAbsolute }

  property("Resolved path's name is prefixed path1's name (with path)") =
    forAll {(p: Path, q: Path) => if (!p.equals(Path(""))) p.resolve(q).startsWith(p) else true }

  property("If path2 is absolute, length of resolved path is path1.segmentCount + path2.segmentCount - 1 (with string)") =
    forAll(genPath, genAbsolutePathString) {
      (path1: Path, path2: String) =>
        if (path1.path == "" && path2 != "/")
          path1.resolve(path2).segmentCount == path1.segmentCount + Path(path2).segmentCount - 2
        else
          path1.resolve(path2).segmentCount == path1.segmentCount + Path(path2).segmentCount - 1
    }

  property("Resolved path is absolute iff path1 is absolute (with string)") =
    forAll {(p: Path, q: String) => p.isAbsolute == p.resolve(q).isAbsolute }

  property("Resolved path's name is prefixed path1's name (with string)") =
    forAll {(p: Path, q: String) => if (!p.equals(Path(""))) p.resolve(q).startsWith(p) else true }


  // "/", both string and path
  property("If path2 is absolute, length of / path is path1.segmentCount + path2.segmentCount - 1") =
    forAll(genPath, genAbsolutePath) {
      (path1: Path, path2: Path) =>
        if  (path1.path == "" && path2.path != "/")
          path1.resolve(path2).segmentCount == path1.segmentCount + path2.segmentCount - 2
        else
          path1.resolve(path2).segmentCount == path1.segmentCount + path2.segmentCount - 1
    }

  property("/ path is absolute iff path1 is absolute") =
    forAll {(p: Path, q: Path) => p.isAbsolute == (p / q).isAbsolute }

  property("/ path's name is prefixed path1's name") =
    forAll {(p: Path, q: Path) => if (!p.equals(Path(""))) (p / q).startsWith(p) else true }

  property("If path2 is absolute, length of / path is path1.segmentCount + path2.segmentCount - 1 (with string)") =
    forAll(genPath, genAbsolutePathString) {
      (path1: Path, path2: String) =>
        if (path1.path == "" && path2 != "/")
          path1.resolve(path2).segmentCount == path1.segmentCount + Path(path2).segmentCount - 2
        else
          path1.resolve(path2).segmentCount == path1.segmentCount + Path(path2).segmentCount - 1
    }

  property("Resolved path is absolute iff path1 is absolute (with string)") =
    forAll {(p: Path, q: String) => p.isAbsolute == (p / q).isAbsolute }

  property("Resolved path's name is prefixed path1's name (with string)") =
    forAll {(p: Path, q: String) => if (!p.equals(Path(""))) (p / q).startsWith(p) else true }

  // relativeTo, both string and path
  property("relativeTo is never absolute") =
    forAll(genAbsolutePath, genAbsolutePath) {(p: Path, q: Path) => !(p relativeTo q).isAbsolute}

  property("relativeTo empty when you are relativeTo yourself") =
    forAll {(p: Path) => (p relativeTo p) equals Path("")}

  property("relativeTo test for absolute/relative, relative/absolute") =
    forAll(genRelativePath, genAbsolutePathString) {(p: Path, q: String) => Prop.throws(classOf[IllegalArgumentException]) { p.relativeTo(q)}}

  property("relativeTo is never absolute (with string)") =
    forAll(genAbsolutePath, genAbsolutePathString) {(p: Path, q: String) => !(p relativeTo q).isAbsolute}

  property("relativeTo test for absolute/relative, relative/absolute (with string)") =
    forAll(genRelativePath, genAbsolutePathString) {(p: Path, q:String) => Prop.throws(classOf[IllegalArgumentException]) { p.relativeTo(q)}}

  // relativize, both string and path
  property("relativize is never absolute") =
    forAll (genAbsolutePath, genAbsolutePath) {(p: Path, q: Path) => !(p relativize q).isAbsolute}

  property("relativize test for absolute/relative, relative/absolute") =
    forAll(genRelativePath, genAbsolutePath) {(p: Path, q:Path) => Prop.throws(classOf[IllegalArgumentException]) { p relativize q }}

  property("relativize empty when you are relativized to yourself") =
    forAll {(p: Path) => (p relativize p) equals Path("")}

  property("relativize is never absolute (with String") =
    forAll (genAbsolutePath, genAbsolutePathString) {(p: Path, q: String) => !(p relativize q).isAbsolute}

  property("relativize test for absolute/relative, relative/absolute (with String)") =
    forAll(genRelativePath, genAbsolutePathString) {(p: Path, q:String) => Prop.throws(classOf[IllegalArgumentException]) { p relativize q }}


  // to URI and URL
  property("toUri is the same as the jpath URI") =
    forAll {(p: Path) => p.toURI == p.jpath.toUri}

  property("toUrl is the same as the jpath URL") =
    forAll {(p: Path) => p.toURL == p.jpath.toUri.toURL}

  //jfile
  property("jfile is same as jpath's file") =
    forAll{(p: Path) => p.jfile == p.jpath.toFile}

  //normalize

  property("Normalized path does not contain a .") =
    forAll{(p: Path) => !p.normalize.segments.contains(Path("."))}

  property("Normalized path does not contain any .. except at the beginning") =
    forAll{(p: Path) => p.normalize.segments.lastIndexOf(Path("..")) == (p.normalize.segments.count(q => q == Path("..")) - 1) }

  property("Normalized path is no longer than the original") =
    forAll{(p: Path) => p.segmentCount >= p.normalize.segmentCount}


  // isAbsolute
  property("First element is root iff isAbsolute") =
    forAll{(p: Path) => if (p == Path("")) true else p.isAbsolute == p.segmentIterator.next.equals(Path("/"))}

  //toAbsolute

  property("toAbsolute is at least as long as original path") =
    forAll{(p: Path) => p.toAbsolute.segmentCount >= p.segmentCount}

  property("toAbsolute makes a path isAbsolute") =
    forAll{(p: Path) => p.toAbsolute.isAbsolute}

  // endsWith, both string and path

  property("All paths end with themselves") =
    forAll{(p:Path) => p.endsWith(p)}


  property("All paths end with themselves (with string)") =
    forAll{(s:String) => Path(s).endsWith(s)}


  // startsWith, both string and path

  property("All absolute paths start with root") =
    forAll(genAbsolutePath){(p: Path) => p.startsWith(Path("/"))}

  property("All paths start with themselves") =
    forAll{(p:Path) => p.startsWith(p)}

  property("All paths start with themselves (with string)") =
    forAll{(s:String) => Path(s).startsWith(s)}

}


trait FileSetupTeardown extends BeforeAndAfterEach { this: Suite =>

  var flagGlobal: Boolean = false
  var testNo: Int = 1
  var (srcGlobal: String, targetGlobal: String, dirsGlobal: ListBuffer[JPath] , filsGlobal: ListBuffer[JPath]) = ("","",new ListBuffer[JPath],new ListBuffer[JPath])

  def setup : (String, String, ListBuffer[JPath], ListBuffer[JPath]) =
  {
    val src = java.nio.file.Files.createTempDirectory("source_" + testNo + "_").toString + "/"
    val target =  java.nio.file.Files.createTempDirectory("target_" + testNo + "_").toString + "/"
    if(testNo == 1)
      println("File trees can be found in: " + src.split("/").init.mkString("/") + "/" + "\nStructure of folder <source/target>_<test number>_ if test fails")
    testNo += 1
    val p = new Path(FileSystems.getDefault.getPath(src))
    val q = new Path(FileSystems.getDefault.getPath(target))
    if(p.exists())
      p.deleteRecursively()
    p.createDirectory()
    if(q.exists())
      q.deleteRecursively()
    q.createDirectory()
    val (dirs,fils) = createFS(p)
    (src, target, dirs, fils)
  }
  def createFS(p: Path) : (ListBuffer[JPath],ListBuffer[JPath]) =
  {
    val start = 1
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

class FileIOSpec extends FlatSpec with FileSetupTeardown {

  behavior of "File System"

  //copyTo test
  it should "1. copy file to target location correctly" in {
    for(i <- filsGlobal.toList)
    {
      val tmp = Path(i)
      val trgt = Path(FileSystems.getDefault.getPath(targetGlobal + i.toString.split("/").last))
      try {
        tmp.copyTo(trgt)
      }
      catch {
        case nsfe: NoSuchFileException => assert(false)
      }
    }
    for(x <- filsGlobal.toList)
    {
      val tmp = new Path(FileSystems.getDefault.getPath(targetGlobal + x.toString.split("/").last))
      assert(tmp.exists() && tmp.isFile() && (tmp.size().get == 0))
    }
    flagGlobal = true
  }

  //moveTo test
  it should "2. moveFile to target location correctly" in {
    for(i <- filsGlobal.toList)
    {
      val tmp = new Path(i)
      try {
        tmp.moveFile(Path(targetGlobal + i.toString.split("/").last))
      }
      catch {
        case nsfe: NoSuchFileException => assert(false)
      }
    }
    for(x <- filsGlobal.toList)
    {
      val tmp = new Path(FileSystems.getDefault.getPath(targetGlobal + x.toString.split("/").last))
      val tmp2 = new Path(x)
      assert(tmp.exists() && tmp.isFile() && tmp2.nonExistent() && (tmp.size().get == 0))
    }
    flagGlobal = true
  }

  //moveDirectory test
  it should "3. move directory to target location correctly" in {
    for(i <- dirsGlobal.toList)
    {
      val tmp = new Path(i)
      try {
        tmp.moveDirectory(Path(targetGlobal + i.toString.split("/").last))
      }
      catch {
        case nsfe: NoSuchFileException => assert(false)
      }
    }
    for(x <- dirsGlobal.toList)
    {
      val tmp = new Path(FileSystems.getDefault.getPath(targetGlobal + x.toString.split("/").last))
      assert(tmp.exists() && tmp.isDirectory())
    }
    flagGlobal = true
  }


  //deleteRecursively test
  it should "4. recursively delete the 'src' directory where the file tree is constructed" in {
    val p = new Path(FileSystems.getDefault.getPath(srcGlobal))
    p.deleteRecursively()
    assert(p.nonExistent())
    flagGlobal = true
  }

  //createTempFile test
  it should "5. create temp file in target and check its existence" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal)).createTempFile("test", ".tmp")
    assert(p.exists())
    flagGlobal = true
  }

  //createTempDir test
  it should "6. create temp dir in target and check its existence" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal)).createTempDir("test")
    assert(p.exists())
    flagGlobal = true
  }

  //delete test
  it should "7. create a temp file then delete it and check its existence" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal)).createTempFile("test", ".tmp")
    p.delete()
    assert(p.nonExistent())
    flagGlobal = true
  }

  //deleteIfExists test
  it should "8. delete a file if it exists else fail" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal)).createTempFile("test", ".tmp")
    p.delete()
    assert(p.nonExistent())
    flagGlobal = true
  }

  //createDirectory
  it should "9. create a directory and check its existence" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal + "test")).createDirectory()
    assert(p.exists())
    flagGlobal = true
  }

  //createFile
  it should "10. create a file and check its existence" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal + "test.tmp")).createFile()
    assert(p.exists())
    flagGlobal = true
  }

  //isSame test
  it should "11. check if the file is the same" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal + "test.tmp")).createFile()
    assert(p.isSame(p))
    flagGlobal = true
  }

  //size test
  it should "12. ensure temp file size is 0" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal + "test.tmp")).createFile()
    assert(p.size().get === 0)
    flagGlobal = true
  }

  //isReadable test
  it should "13. create a temp file and check if it is readable-> true" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal + "test.tmp")).createFile()
    assert(p.isReadable())
    flagGlobal = true
  }

  //isWritable test
  it should "14. create a temp file and check if it is writable-> true" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal + "test.tmp")).createFile()
    assert(p.isWritable())
    flagGlobal = true
  }

  //isExecutable test
  it should "15. create a temp file and check if it is executable-> false" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal + "test.tmp")).createFile()
    assert(!p.isExecutable)
    flagGlobal = true
  }

  //isSymbolicLink test
  it should "16. creates a SymLink using NIO and ensures it is a symbolic link" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal + "tmp.link"))
    val q = new Path(FileSystems.getDefault.getPath(targetGlobal + "test.tmp")).createFile()
    Files.createSymbolicLink(p.jpath,q.jpath)
    assert(p.isSymLink())
    flagGlobal = true
  }

  //checkAccess test=create tmp file(only read & write access) -> ensure READ/WRITE and no EXECUTE
  it should "17. creates a tmp file and checks permissions" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal)).createTempFile("test", ".tmp")
    assert(p.checkAccess(AccessMode.READ) && p.checkAccess(AccessMode.WRITE) && !p.checkAccess(AccessMode.EXECUTE))
    flagGlobal = true
  }

  //access sets access modes for the given path
  it should "18. set the correct access modes" in {
    val p = new Path(FileSystems.getDefault.getPath(targetGlobal)).createTempFile("test", ".tmp")
    val l = List(AccessMode.EXECUTE)
    p.setAccess(l)
    assert(p.checkAccess(AccessMode.EXECUTE))
    flagGlobal = true
  }

  it should "19. not resolve symbolic links in toRealPath iff NOFOLLOW_LINKS option is used " in {
    val p = Path(FileSystems.getDefault.getPath(targetGlobal + "tmp.link"))
    val q = Path(FileSystems.getDefault.getPath(targetGlobal + "testDir/")).createDirectory()
    Files.createSymbolicLink(p.jpath, q.jpath)
    val pChild = p.resolve("targetFile")
    val qChild = q.resolve("targetFile").createFile()
    val shouldFail = Try(pChild.toRealPath(LinkOption.NOFOLLOW_LINKS))
    val shouldSucceed = Try(pChild.toRealPath())
    assert(shouldFail.get.toString == pChild.path)
    assert(shouldSucceed.get.toString == qChild.toRealPath().toString)
    flagGlobal = true
  }

  //setFilePerm test-> sets posix file permissions
//  it should "19. create a file, change posix permissions, ensure they were set correctly" in {
//    val p = new Path(FileSystems.getDefault.getPath(targetGlobal)).createTempFile("test", ".tmp")
//    val s = Set(attribute.PosixFilePermission.GROUP_EXECUTE,attribute.PosixFilePermission.GROUP_READ,attribute.PosixFilePermission.GROUP_WRITE,attribute.PosixFilePermission.OTHERS_EXECUTE,attribute.PosixFilePermission.OTHERS_READ,attribute.PosixFilePermission.OTHERS_WRITE,attribute.PosixFilePermission.OWNER_EXECUTE,attribute.PosixFilePermission.OWNER_READ,attribute.PosixFilePermission.OWNER_WRITE).toSet
//    p.setFilePerm(s)
//    assert(p.checkAccess())
//  }

//  it should "19. correct the case of paths with toRealPath" in {
//    for(i <- filsGlobal.toList) {
//      val equivalentPath = Path(Path(i).path.toUpperCase)
//      if(!(equivalentPath.toRealPath(LinkOption.NOFOLLOW_LINKS) != Path(i)))
//        flagGlobal = false
//      assert(equivalentPath.toRealPath(LinkOption.NOFOLLOW_LINKS) != Path(i))
//    }
//  }




}

