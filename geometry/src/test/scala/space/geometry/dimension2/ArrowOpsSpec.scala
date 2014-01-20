package space.geometry
package dimension2

class ArrowOpsSpec extends org.scalatest.FreeSpec
with space.approximation.testing.ApproximationTesting {

  "The arrow between to points is a ray segment connecting the two points" in
  { assert ( (xy(1,2) → xy(5,4)) === TwoPoints(xy(1,2), xy(5,4)) ) }

  "The arrow from a vector to a line is the shortest segment" in
  { assert ( (xy(3, 1) → DoubleRay(Origin, Angle(Pi/4)))
  =~ (xy(3, 1) → xy(2, 2)) ) }

  "The arrow from a line to a vector is the shortest segment" in
  { assert ( (DoubleRay(Origin, Angle(Pi/4)) → xy(3, 1))
  =~ (xy(2, 2) → xy(3, 1)) ) }
}
