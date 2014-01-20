package space
package svg

import geometry._
import dimension2._
import color._

import java.io.File
import scala.util.Random

object Svg extends App {

  case class Disc(geo: Circle, color: Color) {

    def toXml: xml.Node =
    <circle cx={geo.center.x.toString} cy={geo.center.y.toString}
    r={geo.radius.toString} fill={color.toRGB.toHexString} />

  }

  val triangle = Triangle(xy(80, 400), xy(400, 40), xy(900, 550))

  implicit val random = new Random(97635465457l)

  write(new File("test.svg")) {

    val focus = xy(500, 300)

    <svg width="1000" height="600"/>.copy(child =
      <rect x="0" y="0" width="1000" height="600" fill="white"/>
      +: (
        Stream.fill(3000) {
          val c = triangle.interior.sample()
          val h = (focus -> c).angle.toDouble / (2 * Pi) + .5
          val d = (focus -> c).length / 500
          Disc(Circle(c, radius = 16), HSL(h, 1 - d,.2 + d))
        }.sortBy(disc => (focus -> disc.geo.center).length) ++
        Stream.fill(1000) {
          val c = triangle.sample()
          val color = HSB(.6, .8, .2)
          val r = 4 + random.nextGaussian() * 2
          Disc(Circle(center = c, radius = r), color)
        }
      ).map(_.toXml)
    ).toString()

  }

  def write(file: File)(string: String) {

    val writer = new java.io.PrintWriter(file)

    try { writer.write(string) }
    finally { writer.close() }
  }

}
