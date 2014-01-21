package space.geometry
package dimension2

class LineSegmentLikeSpec extends org.scalatest.FreeSpec
with space.approximation.testing.ApproximationTesting {

  "TwoPoints" - {
    "It can be rotated about a pivot" in assert (
      TwoPoints(xy(3, 1), xy(2, 2))
      .rotate(-Radians.quarterCircle, xy(3, 2))
      =~ TwoPoints(xy(2, 2), xy(3, 3))
    )
  }

  "PointDifference" - {
    "It can be rotated about a pivot" in assert (
      PointDifference(xy(3, 1), xy(-1, 1))
      .rotate(-Radians.quarterCircle, xy(3, 2))
      =~ PointDifference(xy(2, 2), xy(1, 1))
    )
  }

}
