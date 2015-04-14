import sbt._
import Keys._

object Build extends Build {

    lazy val playVersion = "2.3.7"

    lazy val play = "play" %% "play" % playVersion % "provided"
    //
    lazy val root = Project(id = "play-forms-improved", base = file(".")).settings(
        version := playVersion + "_0.0.1-SNAPSHOT",

        libraryDependencies += "com.naumen" %% "forms-dsl" % "0.0.1-SNAPSHOT",

        resolvers += Resolver.sonatypeRepo("releases"),

        organization := "com.naumen",
        libraryDependencies += play,
        mainClass in(Compile, run) := Some("play.core.server.NettyServer")
    )
}
