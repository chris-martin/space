package space.geometry
package dimension2

class RaySpec extends org.scalatest.FreeSpec with ApproximationTesting {

  "PointAndCircleAngle" - {

    "It can be converted to a ray segment of magnitude 1" in {

      val ray: Ray = PointAndCircleAngle(
      source = xy(3, 2), angle = (-Pi / 2).radians)

      val unit: RaySegment = ray.unitSegment

      assert ( unit.length =~ 1 )

      assert (
        unit.toTwoPoints
        =~ TwoPoints(source = xy(3, 2), destination = xy(3, 1))
      )
    }

    "It can be rotated about its source point" in {

      val ray: Ray = PointAndCircleAngle(
      source = xy(4, 5), angle = (Pi/4).radians)

      assert (
        ray.rotate((Pi/4).radians)
        =~ PointAndCircleAngle(source = xy(4, 5), angle = (Pi/2).radians)
      )
    }

  }

}
