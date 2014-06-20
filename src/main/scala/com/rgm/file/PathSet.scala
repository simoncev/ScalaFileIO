package com.rgm.file

import java.nio.file.{SimpleFileVisitor, LinkOption, Files}
import java.nio.file.{Path => JPath, FileSystem => JFileSystem, _}
import scala.collection.mutable.ListBuffer
import java.nio.file.attribute._
import java.io.{File => JFile, IOException}
import com.rgm.file.Path

/**
 * Created by sshivaprasad on 6/18/14.
 */
//
//trait PathSet[Path] extends Traversable[Path] {
//  def foreach[U](f: Path => U)
//}

 class PathSet (root: Path, matcher: PathMatcher, maxDepth: Int, options: LinkOption*) extends Traversable[Path] {

  override def foreach[U](f: (Path => U)): Unit = {
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
                f(Path(dir))
              }
              d-=1
              FileVisitResult.CONTINUE
            }
          }

          override def visitFile(file: JPath,attrs: BasicFileAttributes ) : FileVisitResult = {
            if(matcher.matches(Path(file)))
              f(Path(file))//l += Path(file)
            FileVisitResult.CONTINUE
          }
        })
    }
  }
}
object PathSet {

  def apply(root: Path, matcher: PathMatcher, maxDepth: Int, options: LinkOption*): PathSet =
    new PathSet(root, matcher, maxDepth, options:_*)

}