package com.rgm.file

import org.scalacheck._

import java.nio.file.{Path => JPath, _}
import java.net.URI
import java.util
import scala.util.{Failure, Success, Try}


/**
 * Created by sshivaprasad on 6/4/14.
 */
object SyntaxSpec extends Properties("Path")
{
  import Prop._
  import Generators._

  val zipFile = Paths.get("src/test/resources/dir1.zip")
  val uri = URI.create("jar:file:" + zipFile.toUri.getPath)
  val env:  util.Map[String, String] = new util.HashMap[String, String]()
  Path(zipFile).deleteIfExists()
  env.put("create", "true")
  val zipSystem = FileSystem(FileSystems.newFileSystem(uri, env))

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

  property("Resolved paths must be of same file system") =
    forAll{(s: String, p: Path) => Try(p.resolve(Path(s)(zipSystem))) match {
      case Success(v) => false
      case Failure(e) => true
    } }

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
