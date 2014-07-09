package com.rgm.file

import com.rgm.file.Generators._
import org.scalacheck.Prop._
import org.scalacheck._
import org.scalatest.FlatSpec
import java.nio.file.{Path => JPath}
import scala.language.postfixOps

class PathSpecSpec extends FlatSpec with FileSetupTeardown {

  behavior of "PathSpec"

  val allMatcher = PathMatcher.All

  override def createFS(p: Path) : (Array[JPath],Array[JPath]) =
  {
    var fls = new Array[JPath](0)
    var dirs = new Array[JPath](0)
    (dirs,fls)
  }
  def buildTmpFileTree = {
    val src = Path(srcGlobal)
    val dir1 = Path.createTempDirectory(src, "dir_1_")
    val dir2 = Path.createTempDirectory(src, "dir_2_")
    val dir3 = Path.createTempDirectory(dir1, "dir_3_")
    val dir4 = Path.createTempDirectory(dir2, "dir_4_")

    Path.createTempFile(dir1, "file_1_",".tmp")
    Path.createTempFile(dir1,"file_2_",".tmp")
    Path.createTempFile(dir3, "file_3_",".tmp")
    Path.createTempFile(dir4, "file_4_", ".tmp")
    Path.createTempFile(Path(srcGlobal), "file_5_",".tmp")
  }

  it should "1. PathSpec should find the current state of the file system" in {
    val pathSpec = PathSpec(Path(srcGlobal)) * allMatcher
    val foo = Path.createTempFile(Path(srcGlobal), "foo", ".tmp")
    Path.createTempFile(Path(srcGlobal), "bar", ".tmp")
    Path.createTempFile(Path(srcGlobal), "baz", ".scala")
    var numTmps = 0
    pathSpec.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 3)
    foo.delete()
    numTmps = 0
    pathSpec.foreach(p => numTmps += 1)
    assert(numTmps == 2)
    val newFoo = Path.createTempFile(Path(srcGlobal), "foo", ".tmp")
    numTmps = 0
    pathSpec.foreach(p => numTmps += 1)
    assert(numTmps == 3)
    flagGlobal = true
  }


  it should "2. test should search at exactly the given depth" in {
    //building testing tree
    buildTmpFileTree
    var numTmps = 0
    (PathSpec(Path(srcGlobal)) * allMatcher).foreach((p:Path) => numTmps+=1)
    assert(numTmps==3)
    numTmps = 0
    (PathSpec(Path(srcGlobal)) * allMatcher * allMatcher).foreach((p:Path) => numTmps+=1)
    assert(numTmps==4)
    flagGlobal = true
  }

  it should "3. PathSpec should apply its filter to the elements it finds" in {
    val matcher = PathMatcher(".*.tmp".r)
    val pathSpec = PathSpec(Path(srcGlobal)) * matcher
    Path.createTempFile(Path(srcGlobal), "foo", ".tmp")
    Path.createTempFile(Path(srcGlobal), "bar", ".tmp")
    Path.createTempFile(Path(srcGlobal), "baz", ".scala")
    var numTmps = 0
    pathSpec.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 2)
    flagGlobal = true
  }

  it should "4. test *" in {
    buildTmpFileTree
    val pathSpec = PathSpec(Path(srcGlobal))
    var num = 0
    (pathSpec * PathMatcher.All).foreach((p: Path) => num+=1)
    assert(num==3)
    flagGlobal = true
  }

  it should "5. Does not match root on searches of children" in {
    val pathSpec = PathSpec(Path.createTempDirectory(Path(srcGlobal), "file_1_")) * allMatcher
    var numFound = 0
    pathSpec.foreach((p: Path) => numFound += 1)
    assert(numFound == 0)
    flagGlobal = true
  }

  it should "6. test *** function" in {
    buildTmpFileTree
    val pathSpec = PathSpec(Path(srcGlobal))
    var num = 0
    (pathSpec.***).foreach((p: Path) => num+=1)
    assert(num==9)
    flagGlobal = true
  }

  //.foreach((p: Path) => num+=1)
  it should "7. simple test union function" in {
    buildTmpFileTree
    var num = 0
    val pathSpec = ((PathSpec(Path(srcGlobal)).descendants(""".*dir[^\/]*""".r,10)) +++ (PathSpec(Path(srcGlobal)).descendants(""".*\.tmp""".r,10)))
    pathSpec.foreach((p: Path) => num+=1)
    assert(num == 9)

    flagGlobal = true
  }

  it should "8. Apply filters built of globs to the elements it finds" in {
    val matcher = PathMatcher(srcGlobal + "*.tmp")
    val pathSpec = PathSpec(Path(srcGlobal)) * matcher
    Path.createTempFile(Path(srcGlobal), "foo", ".tmp")
    Path.createTempFile(Path(srcGlobal), "bar", ".tmp")
    Path.createTempFile(Path(srcGlobal), "baz", ".scala")
    val dir1 = Path.createTempDirectory(Path(srcGlobal), "dir1")
    Path.createTempFile(dir1, "foo", ".tmp")
    Path.createTempFile(dir1, "bar", ".tmp")
    Path.createTempFile(dir1, "baz", ".tmp")
    var numTmps = 0
    pathSpec.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 2)

    numTmps = 0
    val pathSpecAllDepths = PathSpec(Path(srcGlobal)) ** (matcher)
    pathSpecAllDepths.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 2)

    flagGlobal = true
  }

  it should "9. Chain several filters together to cherrypick a file" in {
    buildTmpFileTree
    val rootSet = PathSpec(Path(srcGlobal))
    val complexSet = rootSet +++ (rootSet ** (PathMatcher(""".*dir[^\/]*""".r))) +++ (rootSet * allMatcher * PathMatcher(".*file.*".r))
    var numTmps = 0
    complexSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 7)
    val exclusionSet = complexSet --- (rootSet * allMatcher * PathMatcher(srcGlobal + "*/dir_3*"))
    numTmps = 0
    exclusionSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 6)
    flagGlobal = true
  }

  it should "10. Duplicate files which are in the intersection of two sets being unioned" in {
    buildTmpFileTree
    val allSet = PathSpec(Path(srcGlobal)).***
    val children = PathSpec(Path(srcGlobal)) * allMatcher
    val union = allSet +++ children
    var numTmps = 0
    union.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 12)
    flagGlobal = true
  }

  it should "11. exlcudes & up to date system test" in {
    buildTmpFileTree
    var num = 0
    val pathSpec = PathSpec(Path(srcGlobal)).*** --- (PathSpec(Path(srcGlobal)) ** (PathMatcher(""".*\.tmp""".r)))
    Path.createTempDirectory(Path(srcGlobal), "dir_5_")
    pathSpec.foreach((p: Path) => num+=1)
    assert(num == 5)
    flagGlobal = true
  }

  it should "12. Should not touch disk when traversing a SimplePathSpec" in {
    buildTmpFileTree
    val pathSpec = PathSpec(Path(srcGlobal))
    var numTmps = 0
    pathSpec.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 1)
    Path(srcGlobal).deleteRecursively()
    numTmps = 0
    pathSpec.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 1)
    flagGlobal = true
  }

  it should "13. Exclude each different type of set correctly" in {
    buildTmpFileTree
    val allSet = PathSpec(Path(srcGlobal)).*** +++ PathSpec(Path(srcGlobal))
    var numTmps = 0
    allSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 10)
    val noSimplePathSpec = allSet --- PathSpec(Path(srcGlobal))
    numTmps = 0
    noSimplePathSpec.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 9)
    val noFilteredPathSpec = allSet --- (PathSpec(Path(srcGlobal)) * allMatcher)
    numTmps = 0
    noFilteredPathSpec.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 7)
    val nestedExclude = allSet --- noFilteredPathSpec
    numTmps = 0
    nestedExclude.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 3)
    val noCompoundPathSpec = allSet --- (PathSpec(Path(srcGlobal)) +++ nestedExclude)
    numTmps = 0
    noCompoundPathSpec.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 6)
    flagGlobal = true
  }

  it should "14. test a very intricate set of +++, *, **, ***, and ---" in {
    buildTmpFileTree
    val srcPath = PathSpec(Path(srcGlobal))
    var num = 0
    val ps1 = ((((srcPath ***) --- (srcPath ** (PathMatcher(""".*\.tmp""".r))))) +++ ((srcPath ***) --- (srcPath ** (PathMatcher(""".*dir[^\/]*""".r))))) +++ srcPath
    val pathSpec = ps1 --- (ps1 * PathMatcher(""".*\.tmp""".r))
    pathSpec.foreach((p: Path) => num +=1)
    assert(num==5)
    flagGlobal = true
  }


  it should "15. Use slash to build PathSpecs with globs" in {
    val matcher = PathMatcher(srcGlobal + "*.tmp")
    val pathSpec = PathSpec(Path(srcGlobal)) / (srcGlobal + "*.tmp")
    Path.createTempFile(Path(srcGlobal), "foo", ".tmp")
    Path.createTempFile(Path(srcGlobal), "bar", ".tmp")
    Path.createTempFile(Path(srcGlobal), "baz", ".scala")
    var numTmps = 0
    pathSpec.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 2)
    flagGlobal = true
  }

  it should "16. Test the overriden filter method" in {
    buildTmpFileTree
    val srcPath = PathSpec(Path(srcGlobal))
    var num = 0
    val ps1 = (((srcPath ***) --- (srcPath ** (PathMatcher(""".*\.tmp""".r))))) +++ ((srcPath ***) --- (srcPath ** (PathMatcher(""".*dir[^\/]*""".r))))
    val filtered = ps1.filter((p: Path) => PathMatcher(""".*dir[^\/]*""".r).matches(p))
    for (p <- filtered) {
      num+=1
    }
    assert(num==4)
    assert(filtered.isInstanceOf[PathSpec])
    flagGlobal = true
  }

  it should "17. Map Path=>Path returns a PathSpec" in {
    buildTmpFileTree
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val mapped = basePathSpec.map((p: Path) => p / Path("foo"))
    var num = 0
    for (p <- mapped) {
      num+=1
      assert(p endsWith "foo")
    }
    assert(num==9)
    assert(mapped.isInstanceOf[PathSpec])
    flagGlobal = true
  }

  it should "18. Test the laziness of withFilter" in {
    buildTmpFileTree
    val srcPath = (PathSpec(Path(srcGlobal)) ***)
    val fltr = srcPath.withFilter((p: Path) => PathMatcher(""".*dir[^\/]*""".r).matches(p))
    var num1 = 0
    var num2 = 0
    fltr.foreach((p: Path) => num1+=1)
    Path.createTempDirectory(Path(srcGlobal), "dir_9_")
    fltr.foreach((p: Path) => num2+=1)
    assert(num1==4 && num2==5)
    flagGlobal = true
  }

  it should "19. Mapping Path to a non-Path returns a non-path set of the same cardinality" in {
    buildTmpFileTree
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val mapped = basePathSpec.map((p: Path) => p.size)
    var num = 0
    for (p <- mapped) {
      num+=1
    }
    assert(num==9)
    assert(mapped.isInstanceOf[Traversable[Option[Long]]])
    flagGlobal = true
  }

  it should "20. Exclusion of mapped path sets takes into account all possible paths to a file" in {
    buildTmpFileTree
    val srcGlobalPath = Path(srcGlobal)
    val superSet = PathSpec(srcGlobalPath) +++ PathSpec(srcGlobalPath).***
    val mapped = PathSpec(Path("bogusPath")).map(p => Path(srcGlobal))
    val exclusion = superSet --- mapped
    var num = 0
    for (p <- exclusion) {
      num+=1
    }
    assert(num==9)
    flagGlobal = true
  }

  it should "21. Mapped path sets are evaluated lazily if they result in a PathSpec " in {
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val mapped = basePathSpec.map((p: Path) => p / Path("foo"))
    buildTmpFileTree
    var num = 0
    for (p <- mapped) {
      num+=1
    }
    assert(num==9)
    flagGlobal = true
  }

  it should "22. Mapped path sets are evaluated eagerly if they map Path=>non-Path" in {
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val mapped = basePathSpec.map((p: Path) => p.size)
    buildTmpFileTree
    var num = 0
    for (p <- mapped) {
      num+=1
    }
    assert(num==0)
    flagGlobal = true
  }

  it should "23. Test mapping back and forth" in {
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val mapped = basePathSpec.map((p: Path) => p / Path("foo"))
    val mappedBack = mapped.map((p: Path) => p.parent.get)
    buildTmpFileTree
    val mappedBackSeq = mappedBack.toSeq
    val baseSeq = basePathSpec.toSeq
    for (i <- 0 until mapped.count(p => true)) {
      assert(baseSeq(i) == mappedBackSeq(i))
    }
    flagGlobal = true
  }

  it should "24. Use flatMap to collapse resulting collections to a PathSpec in Path=>Traversable[Path] case" in {
    buildTmpFileTree
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val flatMapped = basePathSpec.flatMap(p => List(p, p / Path("foo")))
    var num = 0
    for (p <- flatMapped)
      num+=1
    assert(num==18)
    assert(flatMapped.isInstanceOf[PathSpec])
    flagGlobal = true
  }

  it should "25. FlatMap should collapse resulting collections to Traversable in Path=>Traversable[non-Path] case" in {
    buildTmpFileTree
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val flatMapped = basePathSpec.flatMap(p => List(p.size.get, p.size.get+1))
    var num = 0
    for (p <- flatMapped) {
      num+=1
    }
    assert(num==18)
    assert(flatMapped.isInstanceOf[Traversable[Long]])
    flagGlobal = true
  }

  it should "26. FlatMap should compute lazily in Path=>Traversable[Path] case" in {
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val flatMapped = basePathSpec.flatMap(p => List(p, p / Path("foo")))
    buildTmpFileTree
    var num = 0
    for (p <- flatMapped) {
      num+=1
    }
    assert(num==18)
    flagGlobal = true
  }

  it should "27. FlatMap should compute eagerly in Path=>Traversable[Path] case" in {
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val flatMapped = basePathSpec.flatMap(p => List(p.size.get, p.size.get+1))
    buildTmpFileTree
    var num = 0
    for (p <- flatMapped) {
      num+=1
    }
    assert(num==0)
    flagGlobal = true
  }

  it should "28. Exclude flatMapped PathSpecs" in {
    buildTmpFileTree
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val flat = basePathSpec.flatMap(p => List(p, p/ Path("foo")))
    var num = 0
    (basePathSpec --- flat).foreach((p: Path) => num+=1)
    assert(num==9)
    flagGlobal = true
  }

  it should "29. Collect to PathSpec if partial function maps to a Path" in {
    val parentIfExists = new PartialFunction[Path, Path] {
      def apply(p: Path) = p.parent.get
      def isDefinedAt(p: Path) = p.exists()
    }
    buildTmpFileTree
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val flatMapped = basePathSpec.flatMap(p => List(p, p / Path("foo")))
    val collected = flatMapped.collect(parentIfExists)
    var num = 0
    for (p <- collected) {
      num+=1
    }
    assert(num==9)
    assert(collected.isInstanceOf[PathSpec])
    flagGlobal = true
  }

  it should "30. Collect to a Traversable[B] if partial function doesn't map to Path" in {
    val sizeIfExists = new PartialFunction[Path, Long] {
      def apply(p: Path) = p.size.get
      def isDefinedAt(p: Path) = p.exists()
    }
    buildTmpFileTree
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val flatMapped = basePathSpec.flatMap(p => List(p, p / Path("foo")))
    val collected = flatMapped.collect(sizeIfExists)
    var num = 0
    for (p <- collected) {
      num+=1
    }
    assert(num==9)
    assert(collected.isInstanceOf[Traversable[Long]])
    flagGlobal = true
  }

  it should "31. Evaluate lazily if partial function maps to a Path" in {
    val parentIfExists = new PartialFunction[Path, Path] {
      def apply(p: Path) = p.parent.get
      def isDefinedAt(p: Path) = p.exists()
    }
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val flatMapped = basePathSpec.flatMap(p => List(p, p / Path("foo")))
    val collected = flatMapped.collect(parentIfExists)
    buildTmpFileTree
    var num = 0
    for (p <- collected) {
      num+=1
    }
    assert(num==9)
    assert(collected.isInstanceOf[PathSpec])
    flagGlobal = true
  }

  it should "32. Evaluate eagerly if partial function doesn't map to a Path" in {
    val sizeIfExists = new PartialFunction[Path, Long] {
      def apply(p: Path) = p.size.get
      def isDefinedAt(p: Path) = p.exists()
    }
    val basePathSpec = PathSpec(Path(srcGlobal)).***
    val flatMapped = basePathSpec.flatMap(p => List(p, p / Path("foo")))
    val collected = flatMapped.collect(sizeIfExists)
    buildTmpFileTree
    var num = 0
    for (p <- collected) {
      num+=1
    }
    assert(num==0)
    flagGlobal = true
  }
}

object PathSpecSpec extends Properties("PathSpec") {

  //implicit conversion
  property("paths convert to PathSpecs") =
    forAll{ (p: Path) =>  p.***.isInstanceOf[PathSpec]}

}