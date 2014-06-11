package com.rgm.file
import org.scalacheck._

import org.scalatest._
import scala.language.reflectiveCalls

import java.nio.file.{Path => JPath, _}
import scala.collection.mutable._
import scala.util.Random

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
//  property("simpleName") = forAll{ (p: Path) => p.simpleName == p.path.replaceAll("""\.(.*)""", "")}
//  property("simpleName size check") = forAll{ (p: Path) => p.simpleName.size <= p.path.size}

  //properties for extension function
  property("extension size check") = forAll{ (p: Path) => if (p.extension == None) true else p.extension.get.toString.size < p.name.size}
  property("extension ") = 1 == 1

  //properties for withExtension function
  property("withExtension is simple name plus the extension") = forAll{ (p:Path, s:String) => p.withExtension(Option(s)) == p.simpleName.toString.concat(s) }

//  //properties for segments function
//  property("segments size check") = forAll{ (p: Path) => p.segments.size <= p.path.count(_ == '/') + 1 }
//
//  //properties for segments iterable
//  property("segments each item returned by iterator exists in name") = forAll{ (p: Path) => p.segmentIterator.forall(k => p.name.contains(k.toString))}
//  property("segments each item returned by iterator's size is less than name") = forAll{ (p: Path) => p.segmentIterator.forall(k => p.name.size >= k.toString.size)}
//
//  //properties for segment size
//  property("segmentsCount check") = forAll{ (p: Path) => p.segmentCount == p.segments.size }

  //properties for root
  property("root:p is absolute") = forAll{ (p:Path) => p.isAbsolute == p.root.isDefined}

  //properties for parent
  property("Parent of a normalized path is the same as path + /..") =
    forAll{(p: Path) =>
      if (p.normalize.parent != None || p.normalize.segments.last == Path(".."))
        p.normalize.parent.get == p.normalize.resolve("..").normalize
      else
        true}

  property("Parent is prefix of its path") =
    forAll{(p: Path) => if (p.parent != None) p.startsWith(p.parent.get) else true}

  // sibling, both string and path
  property("Sibling has same parent (called with Path)") =
    forAll { (p: Path, q: Path) => (p.parent == None) || (p.sibling(q).startsWith(p.parent.get)) }

  property("Siblings are both absolute/both relative (called with Path)") =
    forAll {(p: Path, q: Path) => p.isAbsolute == p.sibling(q).isAbsolute }

  property("Sibling has same parent (called with String)") =
    forAll {(p: Path, q: String) => (p.parent == None) || (p.sibling(q).startsWith(p.parent.get))}

  property("Siblings are both absolute/both relative (called with String)") =
    Path("/foo/bar").isAbsolute == Path("/foo/bar").sibling(Path("foo")).isAbsolute

  // resolve, both string and path
  property("If path2 is absolute, length of resolved path is path1.segmentCount + path2.segmentCount - 1 (with path)") =
    forAll(genPath, genAbsolutePath) {
      (path1: Path, path2: Path) => path1.resolve(path2).segmentCount == path1.segmentCount + path2.segmentCount - 1
  }
  property("Resolved path is absolute iff path1 is absolute (with path)") =
    forAll {(p: Path, q: Path) => p.isAbsolute == p.resolve(q).isAbsolute }

  property("Resolved path's name is prefixed path1's name (with path)") =
    forAll {(p: Path, q: Path) => if (!p.equals(Path(""))) p.resolve(q).startsWith(p) else true }

  property("If path2 is absolute, length of resolved path is path1.segmentCount + path2.segmentCount - 1 (with string)") =
    forAll(genPath, genAbsolutePathString) {
      (path1: Path, path2: String) => path1.resolve(path2).segmentCount == path1.segmentCount + Path(path2).segmentCount - 1
    }
  property("Resolved path is absolute iff path1 is absolute (with string)") =
    forAll {(p: Path, q: String) => p.isAbsolute == p.resolve(q).isAbsolute }

  property("Resolved path's name is prefixed path1's name (with string)") =
    forAll {(p: Path, q: String) => if (!p.equals(Path(""))) p.resolve(q).startsWith(p) else true }


  // "/", both string and path
  property("If path2 is absolute, length of / path is path1.segmentCount + path2.segmentCount - 1") =
    forAll(genPath, genAbsolutePath) {
      (path1: Path, path2: Path) => (path1 / path2).segmentCount == path1.segmentCount + path2.segmentCount - 1
    }

  property("/ path is absolute iff path1 is absolute") =
      forAll {(p: Path, q: Path) => p.isAbsolute == (p / q).isAbsolute }

  property("/ path's name is prefixed path1's name") =
    forAll {(p: Path, q: Path) => if (!p.equals(Path(""))) (p / q).startsWith(p) else true }

  property("If path2 is absolute, length of / path is path1.segmentCount + path2.segmentCount - 1 (with string)") =
    forAll(genPath, genAbsolutePathString) {
      (path1: Path, path2: String) => (path1 / path2).segmentCount == path1.segmentCount + Path(path2).segmentCount - 1
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

    // startsWith, both string and path

    property("All absolute paths start with root") =
      forAll(genAbsolutePath){(p: Path) => p.startsWith(Path("/"))}



}

trait FileSetupTeardown extends BeforeAndAfterEach { this: Suite =>
  var dat =
    new {
      var (src, target, dirs, fils) = setup
    }

  def setup : (String, String, ListBuffer[JPath], ListBuffer[JPath]) =
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



  override def beforeEach = {
    try super.beforeEach
    finally{
      var (src, target, dirs, fils) = setup
      dat.src = src
      dat.target = target
      dat.dirs = dirs
      dat.fils = fils
    }
  }

}

class FileIOSpec extends FlatSpec with FileSetupTeardown {

  behavior of "File System"

  //copyTo test
  it should "copy file to target location correctly" in {
    //val fils = main.fils
    //val target = main.target
    for(i <- dat.fils.toList)
    {
      val tmp = new Path(i)
      val trgt = new Path(FileSystems.getDefault.getPath(dat.target + i.toString.split("/").last))
      try {
        tmp.copyTo(trgt)
      }
      catch {
        case nsfe: NoSuchFileException => assert(false)
      }
    }
    for(x <- dat.fils.toList)
    {
      val tmp = new Path(FileSystems.getDefault.getPath(dat.target + x.toString.split("/").last))
      assert(tmp.exists && tmp.isFile && (tmp.size.get == 0))
    }
  }

  //moveTo test
  it should "moveFile to target location correctly" in {
    for(i <- dat.fils.toList)
    {
      val tmp = new Path(i)
      try {
        tmp.moveFile(dat.target + i.toString.split("/").last)
      }
      catch {
        case nsfe: NoSuchFileException => assert(false)
      }
    }
    for(x <- dat.fils.toList)
    {
      val tmp = new Path(FileSystems.getDefault.getPath(dat.target + x.toString.split("/").last))
      val tmp2 = new Path(x)
      assert(tmp.exists && tmp.isFile && tmp2.nonExistent && (tmp.size.get == 0))
    }

  }

  //moveDirectory test
  it should "move directory to target location correctly" in {
    for(i <- dat.dirs.toList)
    {
      val tmp = new Path(i)
      try {
        tmp.moveDirectory(dat.target + i.toString.split("/").last)
      }
      catch {
        case nsfe: NoSuchFileException => assert(false)
      }
    }
    for(x <- dat.dirs.toList)
    {
      val tmp = new Path(FileSystems.getDefault.getPath(dat.target + x.toString.split("/").last))
      assert(tmp.exists && tmp.isDirectory)
    }
  }


  //deleteRecursively test
  it should "recursively delete the 'src' directory where the file tree is constructed" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.src))
    p.deleteRecursively
    assert(p.nonExistent)
  }

  //createTempFile test
  it should "create temp file in target and check its existence" in {
    assert(new Path(FileSystems.getDefault.getPath(dat.target)).createTempFile("test", ".tmp").exists)
  }

  //createTempDir test
  it should "create temp dir in target and check its existence" in {
    assert(new Path(FileSystems.getDefault.getPath(dat.target)).createTempDir("test").exists)
  }

  //delete test
  it should "create a temp file then delete it and check its existence" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target)).createTempFile("test", ".tmp")
    p.delete
    assert(p.nonExistent)
  }

  //deleteIfExists test
  it should "delete a file if it exists else fail" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target)).createTempFile("test", ".tmp")
    val q = new Path(FileSystems.getDefault.getPath(dat.target)).createTempFile("test2", ".tmp")
    p.delete
    assert(!p.deleteIfExists && q.deleteIfExists)
  }

  //createDirectory
  it should "create a directory and check its existence" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target + "test")).createDirectory
    assert(p.exists)
  }

  //createFile
  it should "create a file and check its existence" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target + "test.tmp")).createFile
    assert(p.exists)
  }

  //isSame test
  it should "check if the file is the same" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target + "test.tmp")).createFile
    assert(p.isSame(p))
  }

  //size test
  it should "ensure temp file size is 0" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target + "test.tmp")).createFile
    assert(p.size.get === 0)
  }

  //isReadable test
  it should "create a temp file and check if it is readable-> true" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target + "test.tmp")).createFile
    assert(p.isReadable)
  }

  //isWritable test
  it should "create a temp file and check if it is writable-> true" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target + "test.tmp")).createFile
    assert(p.isWritable)
  }

  //isReadable test
  it should "create a temp file and check if it is executable-> false" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target + "test.tmp")).createFile
    assert(!p.isExecutable)
  }

  //isReadable test
  it should "create a temp file and check if it is symLink-> false" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target + "test.tmp")).createFile
    assert(!p.isSymLink)
  }

  //isSymbolicLink test
  it should "creates a SymLink using NIO and ensures it is a symbolic link" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target + "tmp.link"))
    val q = new Path(FileSystems.getDefault.getPath(dat.target + "test.tmp")).createFile
    Files.createSymbolicLink(p.jpath,q.jpath)
    assert(p.isSymLink)
  }

  //checkAccess test=create tmp file(only read & write access) -> ensure READ/WRITE and no EXECUTE
  it should "creates a tmp file and checks permissions" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target)).createTempFile("test", ".tmp")
    assert(p.checkAccess(AccessMode.READ) && p.checkAccess(AccessMode.WRITE) && !p.checkAccess(AccessMode.EXECUTE))
  }

  //access sets access modes for the given path
  it should "set the correct access modes" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target)).createTempFile("test", ".tmp")
    val l = List(AccessMode.EXECUTE)
    p.setAccess(l)
    assert(p.checkAccess(AccessMode.EXECUTE))
  }

  //setFilePerm test-> sets posix file permissions
  it should "create a file, change posix permissions, ensure they were set correctly" in {
    val p = new Path(FileSystems.getDefault.getPath(dat.target)).createTempFile("test", ".tmp")
    val s = Set(attribute.PosixFilePermission.GROUP_EXECUTE,attribute.PosixFilePermission.GROUP_READ,attribute.PosixFilePermission.GROUP_WRITE,attribute.PosixFilePermission.OTHERS_EXECUTE,attribute.PosixFilePermission.OTHERS_READ,attribute.PosixFilePermission.OTHERS_WRITE,attribute.PosixFilePermission.OWNER_EXECUTE,attribute.PosixFilePermission.OWNER_READ,attribute.PosixFilePermission.OWNER_WRITE).toSet
    p.setFilePerm(s)
    assert(p.checkAccess())
  }

}

