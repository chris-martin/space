package space.geometry
package dimension2

class RaySpec extends org.scalatest.FreeSpec with ApproximationTesting {

  "PointAndCircleAngle" - {

    "It can be converted to a ray segment of magnitude 1" in {

      val ray: Ray = PointAndCircleAngle(
        source = CartesianVector(3, 2),
        angle = CircleRadians(-Pi / 2)
      )

      val unit: RaySegment = ray.unitSegment

      assert ( unit.length =~ 1 )

      assert (
        unit.toTwoPoints =~ TwoPoints(
          source = CartesianVector(3, 2),
          destination = CartesianVector(3, 1)
        )
      )
    }

    "It can be rotated about its source point" in {

      val ray: Ray = PointAndCircleAngle(
        source = CartesianVector(4, 5),
        angle = Angle(Pi/4)
      )

      assert (
        ray.rotate(Angle(Pi/4)) =~ PointAndCircleAngle(
          source = CartesianVector(4, 5),
          angle = Angle(Pi/2)
        )
      )
    }

  }

}
