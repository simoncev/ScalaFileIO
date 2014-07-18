#Overview

A Scala file system API backed by Java NIO.2.  The motivation of the project was to replace the branch of
Jesse Eichar's Scala-IO library (http://jesseeichar.github.io/scala-io-doc/0.4.3/#!/overview)
maintained by Jeff Olson by an easier to maintain and cleaner version while using Java NIO.2 on the backend for improved reliability.


##Path

The basic building block of the library is the Path, built around the Java NIO.2 Path.  
