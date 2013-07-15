package space.geometry
package dimension2

class LineLikeSpec extends org.scalatest.FreeSpec with ApproximationTesting {

  "PointAndSemicircleAngle" - {

    "It can be rotated about its pivot" in {

      val doubleray: DoubleRay = PointAndSemicircleAngle(
        pivot = CartesianVector(4, 5),
        angle = Angle(2*Pi/6)
      )

      assert (
        doubleray.rotate(Angle(-3*Pi/6)) =~
          PointAndSemicircleAngle(
            pivot = CartesianVector(4, 5),
            angle = Angle(Pi-Pi/6)
          )
      )

    }

  }

}
