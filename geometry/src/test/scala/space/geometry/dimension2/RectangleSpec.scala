package space.geometry
package dimension2

class RectangleSpec extends org.scalatest.FreeSpec
    with space.approximation.testing.ApproximationTesting {

  "DiagonalAndCornerRectangle" - {
    "It can be flipped" in
      assert(
        DiagonalAndCornerRectangle(
          diagonal = LineSegment(xy(1, 5), xy(3, 2)),
          corner = xy(1, 2)
        ).flipCorner
        =~
        DiagonalAndCornerRectangle(
          diagonal = LineSegment(xy(1, 5), xy(3, 2)),
          corner = xy(3, 5)
        )
      )
  }
}
