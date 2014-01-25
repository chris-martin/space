package space.geometry
package dimension2

import scala.util.Random

trait SampleOps {

  implicit class SampleVectorFromRaySegment(segment: RaySegment) {

    def sample()(implicit random: Random): Vector =
    segment.source + PolarVector(
      magnitude = random.nextDouble() * segment.length,
      angle = segment.angle
    )
  }

  implicit class SampleVectorFromLineSegment(segment: LineSegment) {

    def sample()(implicit random: Random): Vector =
      segment.arbitrarilyDirected.sample()
  }

  implicit class SamplePointOnACircle(circle: Circle) {

    def sample()(implicit random: Random): Vector =
    circle.center + PolarVector(
      magnitude = circle.radius,
      angle = CircleRadians(random.nextDouble() * twoPi)
    )
  }

  implicit class SamplePointOnATriangle(triangle: Triangle) {

    val perimeter = triangle.arbitrarilyDirected.perimeter

    def sample()(implicit random: Random): Vector =
    perimeter traverse (random.nextDouble() * perimeter.length)
  }

  implicit class SamplePointInsideATriangle(interior: Triangle.Interior) {

    val triangle = interior.triangle.arbitrarilyDirected
    import triangle.{a, b, c}

    /** Shape Distributions (2002) by Robert Osada, Thomas Funkhouser,
      * Bernard Chazelle, David Dobkin.
      * http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.11.8333
      */
    def sample()(implicit random: Random): Vector = {
      val (r1, r2) = (random.nextDouble(), random.nextDouble())
      sqrt(1-r1)*a + sqrt(r1)*(1-r2)*b + sqrt(r1)*r2*c
    }
  }

  implicit class SamplePointInsideARectangle(interior: Rectangle.Interior) {

    def sample()(implicit random: Random): Vector = {
      interior.rectangle match {

        case rectangle: OrthogonalRectangle =>
          xy(rectangle.perimeter.bottom.sample().x,
             rectangle.perimeter.left.sample().y)

        case rectangle =>
          var t = rectangle.arbitraryDiagonalAndCorner
          if (random.nextBoolean()) t = t.flipCorner
          t.interior.sample
      }
    }
  }
}
