package space.geometry
package dimension2

class PolarVectorSpec extends org.scalatest.FunSpec with ApproximationTesting {

  val v = PolarVector(_, _)

  it ("It equals an identical polar vector.") {
    val m = 2.squareRoot
    val a = Angle(Pi/4)
    assert ( v(m, a) =~ v(m, a) )
  }

  it ("It equals itself rotated by 2 Pi.") {
    val x = v(2.squareRoot, Angle(Pi/4))
    assert ( x =~ x.rotate(Angle(2*Pi)) )
  }

  it ("It does not equal a polar vector with a different magnitude") {
    val a = Angle(Pi/4)
    assert ( v(1, a) !=~ v(2, a) )
  }

  it ("It does not equal a polar vector with a different angle") {
    val m = 2.squareRoot
    assert ( v(m, Angle(Pi/4)) !=~ v(m, Angle(Pi/3)) )
  }

  it ("It calculates its X value.") {
    assert ( v(2.squareRoot, Angle(Pi/4)).x =~ 1 )
  }

  it ("It calculates its Y value.") {
    assert ( v(2.squareRoot, Angle(Pi/4)).y =~ 1 )
  }

  it ("It can be negated.") {
    val m = 2.squareRoot
    assert ( -v(m, Angle(Pi/4)) =~ v(m, Angle(-3*Pi/4)) )
  }

}
