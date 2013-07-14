package space.geometry
package dimension2

class LineSpec extends org.scalatest.FreeSpec with ApproximationTesting {

  "PointAndCircleAngle" - {

    "It can be converted to a ray segment of magnitude 1" in {
      val ray = Ray(CartesianVector(3, 2), CircleRadians(-Pi / 2))
      val unit = ray.unitSegment

      assert ( unit.length =~ 1 )

      assert (
        unit.toTwoPoints
        =~ TwoPoints(CartesianVector(3, 2), CartesianVector(3, 1))
      )
    }

    "It can be rotated about its source point" in {
      assert (
        Ray(CartesianVector(4, 5), Angle(Pi/4)).rotate(Angle(Pi/4))
        =~ Ray(CartesianVector(4, 5), Angle(Pi/2))
      )
    }

  }

}
