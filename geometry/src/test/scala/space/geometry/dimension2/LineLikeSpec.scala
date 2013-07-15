package space.geometry
package dimension2

class LineLikeSpec extends org.scalatest.FreeSpec with ApproximationTesting {

  "PointAndSemicircleAngle" - {

    "It can be rotated about its pivot" in {

      val doubleray: DoubleRay = PointAndSemicircleAngle(
      pivot = xy(4, 5), angle = (2*Pi/6).radians )

      assert (
        doubleray.rotate((-3*Pi/6).radians)
        =~ PointAndSemicircleAngle(pivot = xy(4, 5), angle = (Pi-Pi/6).radians)
      )

    }

  }

}
