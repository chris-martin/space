package space.geometry
package dimension2
package angle

class SemicircleRadiansSpec extends org.scalatest.FunSpec with NearEqualityTesting {

  describe("A SemicircleRadians") {

    val a = SemicircleRadians(Pi/4)

    it ("equals an identical angle") { assert ( a =~ SemicircleRadians(Pi/4) ) }
    it ("does not equal a different angle") { assert ( a !=~ SemicircleRadians(Pi/8) ) }

    it ("equals itself plus a half circle") { assert ( a =~ a + Angle.halfCircle ) }
    it ("equals itself minus a half circle") { assert ( a =~ a - Angle.halfCircle ) }
  }

}
