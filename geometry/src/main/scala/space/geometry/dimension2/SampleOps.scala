package space.geometry
package dimension2

import scala.util.Random

trait SampleOps {

  implicit class RaySegmentSampling(segment: RaySegment) {

    def sample()(implicit random: Random): Point =
      segment.source + PolarVector(
        magnitude = random.nextDouble() * segment.length,
        angle = segment.angle
      )
  }

  implicit class LineSegmentSampling(segment: LineSegment) {

    def sample()(implicit random: Random): Point =
      segment.arbitrarilyDirected.sample()
  }

  implicit class CircleSampling(circle: Circle) {

    def sample()(implicit random: Random): Point =
      circle.center + PolarVector(
        magnitude = circle.radius,
        angle = CircleRadians(random.nextDouble() * twoPi)
      )
  }

  implicit class TriangleSampling(triangle: Triangle) {

    def samplePerimeter()(implicit random: Random): Point =
      triangle.arbitraryPath
        .traverse(random.nextDouble() * triangle.perimeterLength)

    /** Shape Distributions (2002) by Robert Osada, Thomas Funkhouser,
      * Bernard Chazelle, David Dobkin.
      * http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.11.8333
      */
    def sampleInterior()(implicit random: Random): Point = {
      val path = triangle.arbitraryPath
      import path.{a, b, c}
      val (r1, r2) = (random.nextDouble(), random.nextDouble())
      sqrt(1-r1)*a + sqrt(r1)*(1-r2)*b + sqrt(r1)*r2*c
    }
  }

  implicit class RectangleSampling(rectangle: Rectangle) {

    def sampleInterior()(implicit random: Random): Point =

      rectangle match {

        case r: RectangleCenterWidthAndHeight =>
          xy( r.bottomEdge.sample().x,
              r.leftEdge  .sample().y )

        case r =>
          var t = r.arbitraryDiagonalAndCornerRectangle
          if (random.nextBoolean()) t = t.flipCorner
          t.sampleInterior()
      }
  }
}
