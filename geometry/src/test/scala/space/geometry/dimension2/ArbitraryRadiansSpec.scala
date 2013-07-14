package space.geometry
package dimension2

class ArbitraryRadiansSpec extends org.scalatest.FunSpec with
ApproximationTesting {

  val a = ArbitraryRadians(Pi/4)

  it ("It equals an identical ArbitraryRadians") {
    assert ( a =~ ArbitraryRadians(Pi/4) )
  }

  it ("It does not equal a different ArbitraryRadians.") {
    assert ( a !=~ ArbitraryRadians(Pi/5) )
  }

  it ("It does not equal itself plus 2 Pi.") {
    assert ( a !=~ a + ArbitraryRadians(2*Pi) )
  }

  it ("It does not equal itself minus 2 Pi.") {
    assert ( a !=~ a - ArbitraryRadians(2*Pi) )
  }

}
