package space.geometry
package dimension2
package angle

class ArbitraryRadiansSpec extends org.scalatest.FunSpec with NearEqualityTesting {

  describe("An ArbitraryRadians") {

    val a = ArbitraryRadians(Pi/4)

    it ("equals an identical ArbitraryRadians") { assert ( a =~ ArbitraryRadians(Pi/4) ) }
    it ("does not equal a different ArbitraryRadians") { assert ( a !=~ ArbitraryRadians(Pi/5) ) }

    it ("does not equal itself plus 2 Pi") { assert ( a !=~ a + ArbitraryRadians(2*Pi) ) }
    it ("does not equal itself minus 2 Pi") { assert ( a !=~ a - ArbitraryRadians(2*Pi) ) }
  }


}
