import sbt._
import Keys._

object Build extends Build {

  lazy val playVersion =  "2.1.3"

  lazy val play =  "play" %% "play" % playVersion
//
  lazy val root = Project(id = "play-forms-improved", base = file(".")).settings(
    version := playVersion + "_0.0.1-SNAPSHOT",

  libraryDependencies += "com.naumen" %% "forms-dsl" % "0.0.1-SNAPSHOT" ,

  libraryDependencies += "org.specs2" %% "specs2" % "2.1.1" % "test",

  scalacOptions in Test ++= Seq("-Yrangepos")           ,


  resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
    "releases"  at "http://oss.sonatype.org/content/repositories/releases")   ,

    organization := "com.naumen",
    libraryDependencies += play,
    mainClass in (Compile, run) := Some("play.core.server.NettyServer")
  )
}