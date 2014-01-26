package space.geometry
package dimension2

class ArrowOpsSpec extends org.scalatest.FreeSpec
    with space.approximation.testing.ApproximationTesting {

  "The arrow between to points is a ray segment connecting the two points" in
    assert ( (xy(1,2) → xy(5,4)) === TwoPoints(xy(1,2), xy(5,4)) )

  "The arrow between a point and a line is the shortest segment" - {

    def test(name: String, point: Point, line: LineLike,
        closestPoint: Point) {

      s"$name, point → line" in
        assert ( (point → line) =~ (point → closestPoint) )

      s"$name, line → point" in
        assert ( (line → point) =~ (closestPoint → point) )
    }

    test(
      "1",
      point = xy(3, 1),
      line = DoubleRay(Origin, Radians(Pi/4)),
      closestPoint = xy(2, 2)
    )
  }
}
