package com.rgm.file

//import java.nio.file.{SimpleFileVisitor, LinkOption, Files}
import java.nio.file.{Path => JPath, FileSystem => JFileSystem, _}
import scala.collection.mutable.ListBuffer
import java.nio.file.attribute._
import java.io.{File => JFile, IOException}


/**
 * Created by sshivaprasad on 6/18/14.
 */
//
//trait PathSet[Path] extends Traversable[Path] {
//  def foreach[U](f: Path => U)
//}

 class PathSet[Path](root: Path, matcher: PathMatcher, maxDepth: Int, options: LinkOption*) extends Traversable[Path] {
  def foreach[U](f: (Path => U)) = {
    PathSet(Path(root.toString), matcher, maxDepth, options:_*)
  }
}
object PathSet {

  def apply(root: Path, matcher: PathMatcher, maxDepth: Int, options: LinkOption*): Traversable[Path] = {
    val l: ListBuffer[Path] = new ListBuffer[Path]
    var d: Int = maxDepth
    if(root.exists()) {
      Files.walkFileTree(root.jpath,
      new SimpleFileVisitor[JPath] {
        override def preVisitDirectory(dir: JPath, attrs: BasicFileAttributes) : FileVisitResult = {
          if(d <= 0) {
            d = maxDepth
            return FileVisitResult.SKIP_SUBTREE
          }
          else {
            if(matcher.matches(Path(dir))) {
              l+= Path(dir)
            }
            d-=1
            FileVisitResult.CONTINUE
          }
        }

        override def visitFile(file: JPath,attrs: BasicFileAttributes ) : FileVisitResult = {
          if(matcher.matches(Path(file))) l += Path(file)
          FileVisitResult.CONTINUE
        }
      })
    }
    l.toTraversable
  }
}
