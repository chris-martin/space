package space.geometry
package dimension2

class ArrowOpsSpec extends org.scalatest.FreeSpec
    with space.approximation.testing.ApproximationTesting {

  "The arrow between to points is a ray segment connecting the two points" in
    assert ( (xy(1,2) → xy(5,4)) === TwoPoints(xy(1,2), xy(5,4)) )

  "The arrow between a vector and a line is the shortest segment" - {

    def test(name: String, vector: Vector, line: LineLike,
        closestPoint: Vector) {

      s"$name, vector → line" in
        assert ( (vector → line) =~ (vector → closestPoint) )

      s"$name, line → vector" in
        assert ( (line → vector) =~ (closestPoint → vector) )
    }

    test(
      "1",
      vector = xy(3, 1),
      line = DoubleRay(Origin, Radians(Pi/4)),
      closestPoint = xy(2, 2)
    )
  }
}
