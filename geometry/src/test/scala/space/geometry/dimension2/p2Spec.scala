package space.geometry
package dimension2

class p2Spec extends org.scalatest.FreeSpec
    with space.approximation.ApproximationAssertions {

  "TwoPoints" - {
    "It can be rotated about a pivot" in assert (
      TwoPoints( xy(3, 1), xy(2, 2) )
        .rotate(-Radians.quarterCircle, xy(3, 2))
        ==~ TwoPoints( xy(2, 2), xy(3, 3) )
    )
  }

  "PointDifference" - {
    "It can be rotated about a pivot" in assert (
      PointDifference( xy(3, 1), xy(-1, 1) )
        .rotate(-Radians.quarterCircle, xy(3, 2))
        ==~ PointDifference( xy(2, 2), xy(1, 1) )
    )
  }

  "PointAndSemicircleAngle" - {

    "It can be rotated about its pivot" in {

      val doubleray: DoubleRay =
        PointAndSemicircleAngle(
          pivot = xy(4, 5),
          angle = (2*Pi/6).radians
        )

      assert (
        doubleray.rotate((-3*Pi/6).radians)
        ==~ PointAndSemicircleAngle(
          pivot = xy(4, 5),
          angle = (Pi-Pi/6).radians
        )
      )
    }
  }

  "PointAndCircleAngle" - {

    "It can be converted to a ray segment" - {

      val ray: Ray =
        PointAndCircleAngle(
          source = xy(3, 2),
          angle = (-Pi / 2).radians
        )

      val segment: RaySegment = ray.segmentWithLength(2)

      "with the specified length" in assert ( segment.length ==~ 2 )

      "resulting in the correct segment points" in
        assert ( segment.toTwoPoints ==~ TwoPoints( xy(3, 2), xy(3, 0) ) )
    }

    "It can be rotated about its source point" in {

      val ray: Ray =
        PointAndCircleAngle(
          source = xy(4, 5),
          angle = (Pi/4).radians
        )

      assert (
        ray.rotate((Pi/4).radians)
          ==~ PointAndCircleAngle(
          source = xy(4, 5),
          angle = (Pi/2).radians
        )
      )
    }
  }
}
