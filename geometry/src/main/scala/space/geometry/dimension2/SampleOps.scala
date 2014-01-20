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
      (1-r1.squareRoot)*a + r1.squareRoot*(1-r2)*b + r1.squareRoot*r2*c
    }
  }
}
