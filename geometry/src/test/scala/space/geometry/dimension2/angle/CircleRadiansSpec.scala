package space.geometry
package dimension2
package angle

class CircleRadiansSpec extends org.scalatest.FunSpec with NearEqualityTesting {

  describe("A CircleRadians") {

    val a = CircleRadians(Pi/4)

    it ("equals an identical CircleRadians") { assert ( a =~ CircleRadians(Pi/4) ) }
    it ("does not equal a different CircleRadians") { assert ( a !=~ CircleRadians(Pi/8) ) }

    it ("equals itself plus a circle") { assert ( a =~ a + Angle.circle ) }
    it ("equals itself minus a circle") { assert ( a =~ a - Angle.circle ) }
  }

}
