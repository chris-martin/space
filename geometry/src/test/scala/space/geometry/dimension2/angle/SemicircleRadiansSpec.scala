package space.geometry
package dimension2
package angle

class SemicircleRadiansSpec extends org.scalatest.FunSpec with NearEqualityTesting {

  val a = SemicircleRadians(Pi/4)

  it ("It equals an identical SemicircleRadians.") {
    assert ( a =~ SemicircleRadians(Pi/4) )
  }

  it ("It does not equal a different SemicircleRadians.") {
    assert ( a !=~ SemicircleRadians(Pi/8) )
  }

  it ("It equals itself plus a half circle.") {
    assert ( a =~ a + Angle.halfCircle )
  }

  it ("It equals itself minus a half circle.") {
    assert ( a =~ a - Angle.halfCircle )
  }

}
