package com.rgm.file

import java.nio.file.{Path =>JPath, _}
import org.scalatest.{FlatSpec, Suite, BeforeAndAfterEach}
import scala.collection.mutable.ListBuffer
import scala.util.{Try, Random}
import java.net.URI
import java.util
import scala.language.postfixOps


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
    val dir1 = Path.createTempDir(src, "dir_1_")
    val dir2 = Path.createTempDir(src, "dir_2_")
    val dir3 = Path.createTempDir(dir1, "dir_3_")
    val dir4 = Path.createTempDir(dir2, "dir_4_")

    Path.createTempFile(dir1, "file_1_",".tmp")
    Path.createTempFile(dir1,"file_2_",".tmp")
    Path.createTempFile(dir3, "file_3_",".tmp")
    Path.createTempFile(dir4, "file_4_", ".tmp")
    Path.createTempFile(Path(srcGlobal), "file_5_",".tmp")
  }

  it should "1. PathSet should find the current state of the file system" in {
    val pathSet = PathSet(Path(srcGlobal)) * allMatcher
    val foo = Path.createTempFile(Path(srcGlobal), "foo", ".tmp")
    Path.createTempFile(Path(srcGlobal), "bar", ".tmp")
    Path.createTempFile(Path(srcGlobal), "baz", ".scala")
    var numTmps = 0
    pathSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 3)
    foo.delete()
    numTmps = 0
    pathSet.foreach(p => numTmps += 1)
    assert(numTmps == 2)
    val newFoo = Path.createTempFile(Path(srcGlobal), "foo", ".tmp")
    numTmps = 0
    pathSet.foreach(p => numTmps += 1)
    assert(numTmps == 3)
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
    assert(numTmps==4)
    flagGlobal = true
  }

  it should "3. PathSet should apply its filter to the elements it finds" in {
    val matcher = PathMatcher(".*.tmp".r)
    val pathSet = PathSet(Path(srcGlobal)) * matcher
    Path.createTempFile(Path(srcGlobal), "foo", ".tmp")
    Path.createTempFile(Path(srcGlobal), "bar", ".tmp")
    Path.createTempFile(Path(srcGlobal), "baz", ".scala")
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
    val pathSet = PathSet(Path.createTempDir(Path(srcGlobal), "file_1_")) * allMatcher
    var numFound = 0
    pathSet.foreach((p: Path) => numFound += 1)
    assert(numFound == 0)
    flagGlobal = true
  }

  it should "6. test *** function" in {
    buildTmpFileTree
    val pathSet = PathSet(Path(srcGlobal))
    var num = 0
    (pathSet.***).foreach((p: Path) => num+=1)
    assert(num==9)
    flagGlobal = true
  }

  //.foreach((p: Path) => num+=1)
  it should "7. simple test union function" in {
    buildTmpFileTree
    var num = 0
    val pathSet = ((PathSet(Path(srcGlobal)) ** (""".*dir[^\/]*""".r,10)) +++ (PathSet(Path(srcGlobal)) ** (""".*\.tmp""".r,10)))
    pathSet.foreach((p: Path) => num+=1)
    assert(num == 9)

    flagGlobal = true
  }

  it should "8. Apply filters built of globs to the elements it finds" in {
    val matcher = PathMatcher(srcGlobal + "*.tmp")
    val pathSet = PathSet(Path(srcGlobal)) * matcher
    Path.createTempFile(Path(srcGlobal), "foo", ".tmp")
    Path.createTempFile(Path(srcGlobal), "bar", ".tmp")
    Path.createTempFile(Path(srcGlobal), "baz", ".scala")
    val dir1 = Path.createTempDir(Path(srcGlobal), "dir1")
    Path.createTempFile(dir1, "foo", ".tmp")
    Path.createTempFile(dir1, "bar", ".tmp")
    Path.createTempFile(dir1, "baz", ".tmp")
    var numTmps = 0
    pathSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 2)

    numTmps = 0
    val pathSetAllDepths = PathSet(Path(srcGlobal)) ** matcher
    pathSetAllDepths.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 2)

    flagGlobal = true
  }

  it should "9. Chain several filters together to cherrypick a file" in {
    buildTmpFileTree
    val rootSet = PathSet(Path(srcGlobal))
    val complexSet = rootSet +++ (rootSet ** PathMatcher(""".*dir[^\/]*""".r)) +++ (rootSet * allMatcher * PathMatcher(".*file.*".r))
    var numTmps = 0
    complexSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 7)
    val exclusionSet = complexSet --- (rootSet * allMatcher * PathMatcher(srcGlobal + "*/dir_3*"))
    numTmps = 0
    exclusionSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 6)
  }

  it should "10. Duplicate files which are in the intersection of two sets being unioned" in {
    buildTmpFileTree
    val allSet = PathSet(Path(srcGlobal)).***
    val children = PathSet(Path(srcGlobal)) * allMatcher
    val union = allSet +++ children
    var numTmps = 0
    union.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 12)
  }

  it should "11. exlcudes & up to date system test" in {
    buildTmpFileTree
    var num = 0
    val pathSet = PathSet(Path(srcGlobal)).*** --- (PathSet(Path(srcGlobal)) ** PathMatcher(""".*\.tmp""".r))
    println(pathSet.isInstanceOf[ExclusionPathSet])
    Path.createTempDir(Path(srcGlobal), "dir_5_")
    pathSet.foreach((p: Path) => num+=1)
    assert(num == 5)
    flagGlobal = true
  }

  it should "12. Touch disk when traversing a SimplePathSet" in {
    buildTmpFileTree
    val pathSet = PathSet(Path(srcGlobal))
    var numTmps = 0
    pathSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 1)
    Path(srcGlobal).deleteRecursively()
    numTmps = 0
    pathSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 0)
  }

  it should "13. Exclude each different type of set correctly" in {
    buildTmpFileTree
    val allSet = PathSet(Path(srcGlobal)).*** +++ PathSet(Path(srcGlobal))
    var numTmps = 0
    allSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 10)
    val noSimplePathSet = allSet --- PathSet(Path(srcGlobal))
    numTmps = 0
    noSimplePathSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 9)
    val noFilteredPathSet = allSet --- (PathSet(Path(srcGlobal)) * allMatcher)
    numTmps = 0
    noFilteredPathSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 7)
    val nestedExclude = allSet --- noFilteredPathSet
    numTmps = 0
    nestedExclude.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 3)
    val noCompoundPathSet = allSet --- (PathSet(Path(srcGlobal)) +++ nestedExclude)
    numTmps = 0
    noCompoundPathSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 6)

  }

  it should "14. test a very intricate set of +++, *, **, ***, and ---" in {
    buildTmpFileTree
    val srcPath = PathSet(Path(srcGlobal))
    var num = 0
    val ps1 = ((((srcPath ***) --- (srcPath ** PathMatcher(""".*\.tmp""".r)))) +++ ((srcPath ***) --- (srcPath ** PathMatcher(""".*dir[^\/]*""".r)))) +++ srcPath
    val pathSet = ps1 --- (ps1 * PathMatcher(""".*\.tmp""".r))
    pathSet.foreach((p: Path) => num +=1)
    assert(num==5)
    flagGlobal = true
  }


  it should "15. Use slash to build PathSets with globs" in {
    val matcher = PathMatcher(srcGlobal + "*.tmp")
    val pathSet = PathSet(Path(srcGlobal)) / (srcGlobal + "*.tmp")
    Path.createTempFile(Path(srcGlobal), "foo", ".tmp")
    Path.createTempFile(Path(srcGlobal), "bar", ".tmp")
    Path.createTempFile(Path(srcGlobal), "baz", ".scala")
    var numTmps = 0
    pathSet.foreach((p:Path) => numTmps+=1)
    assert(numTmps == 2)
  }

  it should "16. Test the overriden filter method" in {
    val srcPath = PathSet(Path(srcGlobal))
    var num = 0
    val ps1 = (((srcPath ***) --- (srcPath ** PathMatcher(""".*\.tmp""".r)))) +++ ((srcPath ***) --- (srcPath ** PathMatcher(""".*dir[^\/]*""".r)))
    val filtered = ps1.filter((p: Path) => PathMatcher(""".*dir[^\/]*""".r).matches(p))

    buildTmpFileTree
    for (p <- filtered) {
      num+=1
    }
    assert(num==4)
    flagGlobal = true
  }

  it should "17. Map Path=>Path returns a PathSet" in {
    buildTmpFileTree
    val basePathSet = PathSet(Path(srcGlobal)) ***
    assert(basePathSet.map((p: Path) => p / Path("foo")).isInstanceOf[PathSet])

  }
}