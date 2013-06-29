import sbt._
import Keys._

object Build extends Build {

  type Settings = Seq[Setting[_]]

  def project(id: String, base: String)
             (settings: Settings = Seq()): Project =
    Project(
      id = id, 
      base = file(base),
      settings = Project.defaultSettings ++ Seq(
        scalaVersion := "2.10.2",
        libraryDependencies ++= Seq(
          "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
        )
      ) ++ settings
    )

  lazy val root = project("root", ".") { Seq() }

}
