package space.geometry
package dimension2

class CircleRadiansSpec extends org.scalatest.FunSpec with NearEqualityTesting {

  val a = CircleRadians(Pi/4)

  it ("It equals an identical CircleRadians.") {
    assert ( a =~ CircleRadians(Pi/4) )
  }

  it ("It does not equal a different CircleRadians.") {
    assert ( a !=~ CircleRadians(Pi/8) )
  }

  it ("It equals itself plus a circle.") {
    assert ( a =~ a + Angle.circle )
  }

  it ("It equals itself minus a circle.") {
    assert ( a =~ a - Angle.circle )
  }

}
