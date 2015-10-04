package space
package svg

import geometry._
import dimension2._
import color._

import java.io.File
import scala.util.Random

object Svg extends App {

  case class SvgCircle(geo: Circle, color: Color) {

    def toXml: xml.Node =
      <circle cx={geo.center.x.toString} cy={geo.center.y.toString}
        r={geo.radius.toString} fill={color.toRGB.toHexString} />
  }

  case class SvgOrthogonalRectangle(
      geo: OrthogonalRectangle, color: Color) {

    def toXml: xml.Node =
      <rect x={geo.min.x.toString} y={geo.min.y.toString}
        width={geo.size.x.toString} height={geo.size.y.toString}
        fill={color.toRGB.toHexString} />
  }

  case class Tile(cycle: Cycle, matchingRules: Seq[Int]) {

  }

  val P3tiles = Seq(
    Tile(
      cycle = {
        val r = OrthogonalRhombusByVerticalDiagonalAngleAndEdgeLength(
          center = Origin,
          verticalDiagonalAngle = 108.degrees,
          edgeLength = 1
        )
        Cycle(r.top, r.right, r.bottom, r.left)
      },
      matchingRules = Seq(0, 1, 2, 3)
    ),
    Tile(
      cycle = {
        val r = OrthogonalRhombusByVerticalDiagonalAngleAndEdgeLength(
          center = Origin,
          verticalDiagonalAngle = 144.degrees,
          edgeLength = 1
        )
        Cycle(r.top, r.right, r.bottom, r.left)
      },
      matchingRules = Seq(0, 1, 2, 3)
    )
  )

  implicit val random = new Random(97635465457l)

  write(new File("test.svg")) {

    val boundary = OrthogonalRectangle( xy(0, 0) -- xy(400, 900) )

    <svg width={boundary.size.x.toString}
         height={boundary.size.y.toString}/>.copy(child =
      SvgOrthogonalRectangle(boundary pad 1, RGB hex "111").toXml
    ).toString()

  }

  def write(file: File)(string: String) {

    println(string)

    val writer = new java.io.PrintWriter(file)

    try { writer.write(string) }
    finally { writer.close() }
  }

}

/*
  Stream.fill(1000) {
    val c = triangle.sample()
    val color = HSB(.6, .8, .2)
    val r = 4 + random.nextGaussian() * 2
    Disc(Circle(center = c, radius = r), color)
  }
 */
