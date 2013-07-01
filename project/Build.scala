import sbt._
import Keys._

object Build extends Build {

  type Settings = Seq[Setting[_]]

  def project(id: String, base: String, settings: Settings = Seq()): Project =
    Project(
      id = id, 
      base = file(base),
      settings = Project.defaultSettings ++ Seq(
        scalaVersion := "2.10.2",
        scalacOptions := Seq("-language:implicitConversions"),
        libraryDependencies ++= Seq(
          "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
        )
      ) ++ settings
    )

  lazy val root = project("root", ".") aggregate geometry

  lazy val geometry = project("geometry", "geometry")

  lazy val jogl = project("jogl", "jogl")

}
