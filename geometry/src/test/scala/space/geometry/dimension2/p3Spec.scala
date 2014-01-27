package space.geometry
package dimension2

class p3Spec extends org.scalatest.FreeSpec
    with space.approximation.ApproximationAssertions {

  "ThreePoints" - {

    "It calculates its circumcenter." in assert (
      △(Origin, xy(1, 0), xy(0, 1)).circumcenter
      ==~ Some(xy(0.5, 0.5))
    )

    "It has no circumcenter if all three points are colinear." in assert (
      △(Origin, xy(0, 1), xy(0, 2)).circumcenter
      ==~ None
    )

    "It calculates the length of its perimeter" in assert (
      △(Origin, xy(0, 1), xy(1, 0)).perimeter.length
      ==~ 2 + sqrt(2)
    )

    "It calculates its area" in assert (
      △(Origin, xy(0, 1), xy(1, 0)).area
      ==~ .5
    )
  }
}
