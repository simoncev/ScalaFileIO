
name := "zpath"

description := "A Scala file system API back by Java NIO.2"

organization := "com.rgm"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-optimize", "-unchecked", "-deprecation", "-feature")

crossScalaVersions := Seq("2.10.4", "2.11.1")

publishTo := {
  val repo = url("http://ivy.rgmadvisors.com:8081/artifactory/" + (if (isSnapshot.value) "libs-snapshot-local" else "libs-release-local"))
  Some(Resolver.url("rgm", repo)(Resolver.ivyStylePatterns))
}

publishMavenStyle := false

// Please don't add Artifactory credentials here! Put them in ~/.sbt/0.13/credentials.sbt instead.

sbtrelease.ReleasePlugin.releaseSettings

