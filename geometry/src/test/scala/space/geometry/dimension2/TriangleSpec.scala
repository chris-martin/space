package space.geometry
package dimension2

class TriangleSpec extends org.scalatest.FreeSpec
with space.approximation.testing.ApproximationTesting {

  "ThreePoints" - {

    "It calculates its circumcenter." in
    { assert ( △(Origin, xy(1, 0), xy(0, 1)).circumcenter
      =~ Some(xy(0.5, 0.5)) ) }

    "It has no circumcenter if all three points are colinear." in
    { assert ( △(Origin, xy(0, 1), xy(0, 2)).circumcenter =~ None ) }

    "It calculates the length of its perimeter" in
    { assert ( △(Origin, xy(0, 1), xy(1, 0)).perimeter.length
      =~ 2 + 2.squareRoot ) }

    "It calculates the area of its interior" in
    { assert ( △(Origin, xy(0, 1), xy(1, 0)).interior.area =~ .5 ) }
  }
}
