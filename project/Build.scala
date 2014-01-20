import sbt._
import Keys._

object Build extends Build {

  type Settings = Seq[Setting[_]]

  def project(id: String, base: String, settings: Settings = Seq()): Project =
  Project(
    id = id,
    base = file(base),
    settings = Project.defaultSettings ++ Seq(
      scalaVersion := "2.10.3",
      scalacOptions := Seq("-language:implicitConversions"),
      libraryDependencies ++= Seq(
        "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
      )
    ) ++ settings
  )

  lazy val root = (
    project("root", ".")
    aggregate (approximation, geometry, color)
  )

  lazy val approximation = project("approximation", "approximation")

  lazy val approximationTesting = (
    project("approximation-testing", "approximation/testing")
    dependsOn approximation
  )

  lazy val geometry = (
    project("geometry", "geometry")
    dependsOn (approximation, approximationTesting % "test")
  )

  lazy val color = (
    project("color", "color")
    dependsOn (approximation, approximationTesting % "test")
  )

  lazy val jogl = project("jogl", "jogl")

}
