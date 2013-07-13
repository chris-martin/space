package space.geometry
package dimension2

class TwoPointLineSegmentSpec extends org.scalatest.FunSpec with NearEqualityTesting {

  val v = CartesianVector(_, _)

  it ("It calculates its length") {
    assert ( TwoPointLineSegment(v(2, 3), v(3, 4)).length =~ 2.squareRoot )
  }

}
