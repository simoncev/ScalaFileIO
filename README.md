#Overview

A Scala file system API backed by Java NIO.2.  The motivation of the project was to replace the branch of
Jesse Eichar's Scala-IO library (http://jesseeichar.github.io/scala-io-doc/0.4.3/#!/overview)
maintained by Jeff Olson by an easier to maintain and cleaner version while using Java NIO.2 on the backend for improved reliability.


##Path

The basic building block of the library is the Path, built around the Java NIO.2 Path. The library was organized to mimic NIO.2's
disk access patterns--wherever possible, the library avoids converting paths into real or absolute paths and the accompanying hits
to disk, and when disk accesses are necessary it goes to disk once.

In Scala it is stylistically encouraged to denote methods with side effects with a trailing "()", even when unnecessary, and drop said
parentheses from 

Example:

```scala
val path1: Path = Path("/Users/zaphod/Documents/SpaceshipConfig.scala")
val path2: Path = path1.sibling("HyperdriveConfig.scala")
val path2Stream = path2.outputStream()
//write stuff to file...path2Stream is a Java NIO OutputStream
path2Stream.close()
```

##PathMatcher

PathMatchers contain a predicator which is used by they use to classify Paths as matching a pattern or not.  PathMatchers are primarily
used in PathSpecs to filter out certain files in the file tree underneath a certain folder.  

##PathSpec 

PathSpec is based around the Scala-IO PathSet.  PathSpecs are lazily evaluated Traversables.  A PathSpec is best thought of not as a 
static set of paths but similar to a view of the underlying file system:  except for PathSpecs formed from an array of literal Paths,
the PathSpec will look at disk to determine its membership, which it determines based on the properties it is given.  Like PathSets,
 PathSpecs can contain duplicates of the same element.  PathSpecs can be unioned or exclusioned, and can look at and apply filters
 to paths up to a certain depth below 

Example:

```scala
val pathLiterals = PathSpec(Path("/Users/zaphod"), Path("/Users/trillian"))
for (path <- pathLiteral) {
  println(path) //executes for each literal even if it isn't in the file system
}
val childrenDirectories = pathLiteral * PathMatcher(_.isDirectory())
for (path <- pathLiteral) {
  println(path) //executes on paths that exist on disk and conform to the properties specified
}

```
