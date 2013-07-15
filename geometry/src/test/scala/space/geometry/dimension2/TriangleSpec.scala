package space.geometry
package dimension2

class TriangleSpec extends org.scalatest.FreeSpec with ApproximationTesting {

  "ThreePoints" - {

    "It can be constructed as △(_,_,_) or (_→_→_)" in
    { val a = xy(23, 28); val b = xy(-34, 23.12); val c = xy(0, -0.56)
      assert( △(a, b, c) === (a→b→c) ) }

    "It calculates its circumcenter." in
    {
      assert (
        (Origin → xy(1, 0) → xy(0, 1)).circumcenter =~ Some(xy(0.5, 0.5))
      )
    }

    "It has no circumcenter if all three points are colinear." in
    { assert ( (Origin → xy(0, 1) → xy(0, 2)).circumcenter =~ None ) }

  }

}
