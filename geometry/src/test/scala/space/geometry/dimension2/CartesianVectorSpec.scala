package space.geometry
package dimension2

class CartesianVectorSpec extends org.scalatest.FunSpec with NearEqualityTesting {

  val v = CartesianVector(_, _)

  it ("It equals an identical cartesian vector.") {
    assert ( v(3, 4) =~ v(3, 4) )
  }

  it ("It does not equal a vector with a different X.") {
    assert ( v(3, 4) !=~ v(30, 4) )
  }

  it ("It does not equal a vector with a different Y.") {
    assert ( v(3, 4) !=~ v(3, 40) )
  }

  it ("It calculates its magnitude.") {
    assert ( v(3, 4).magnitude =~ 5 )
  }

  it ("It calculates its angle.") {
    assert ( v(0.5, -(3.squareRoot/2)).angle =~ CircleRadians(5*math.Pi/3) )
  }

  it ("Its angle is invariant under change in magnitude.") {
    val x = v(0.935234, 3.353)
    assert ( x.angle =~ (x*713).angle )
  }

  it ("It can be negated.") {
    assert ( -v(3, 4) =~ v(-3, -4) )
  }

  it ("It can be added to another cartesian vector.") {
    assert ( v(3, 4) + v(1, 3) =~ v(4, 7) )
  }

  it ("It can be subtracted from another cartesian vector.") {
    assert ( v(3, 4) - v(1, 3) =~ v(2, 1) )
  }

  it ("Its magnitude can be multiplied by a scalar.") {
    assert ( v(3, 4) * 1.5 =~ v(4.5, 6) )
  }

  it ("Its magnitude can be divided by a scalar.") {
    assert ( v(3, 4) / 2 =~ v(1.5, 2) )
  }

}
