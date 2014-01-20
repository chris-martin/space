package space.geometry
package dimension2

class VectorSpec extends org.scalatest.FreeSpec
with space.approximation.testing.ApproximationTesting {

  "CartesianVector" - {

    "It equals an identical cartesian vector." in
    { assert ( xy(3, 4) =~ xy(3, 4) ) }

    "It does not equal a vector with a different X." in
    { assert ( xy(3, 4) !=~ xy(30, 4) ) }

    "It does not equal a vector with a different Y." in
    { assert ( xy(3, 4) !=~ xy(3, 40) ) }

    "It calculates its magnitude." in
    { assert ( xy(3, 4).magnitude =~ 5 ) }

    "It calculates its angle." in
    { val x = xy(0.5, -(3.squareRoot/2))
      assert ( x.angle =~ CircleRadians(5*math.Pi/3) ) }

    "Its angle is invariant under change in magnitude." in
    { val x = xy(0.935234, 3.353)
      assert ( x.angle =~ (x*713).angle ) }

    "It can be negated." in
    { assert ( -xy(3, 4) =~ xy(-3, -4) ) }

    "It can be added to another cartesian vector." in
    { assert ( xy(3, 4) + xy(1, 3) =~ xy(4, 7) ) }

    "It can be subtracted from another cartesian vector." in
    { assert ( xy(3, 4) - xy(1, 3) =~ xy(2, 1) ) }

    "Its magnitude can be multiplied by a scalar." in
    { assert ( xy(3, 4) * 1.5 =~ xy(4.5, 6) ) }

    "Its magnitude can be divided by a scalar." in
    { assert ( xy(3, 4) / 2 =~ xy(1.5, 2) ) }
  }

  "PolarVector" - {
  
    val v = PolarVector(_, _)
    
    "It equals an identical polar vector." in
    { val m = 2.squareRoot
      val a = Angle(Pi/4)
      assert ( v(m, a) =~ v(m, a) ) }
    
    "It equals itself rotated by 2 Pi." in
    { val x = v(2.squareRoot, Angle(Pi/4))
      assert ( x =~ x.rotate(Angle(2*Pi)) ) }
    
    "It does not equal a polar vector with a different magnitude" in
    { val a = Angle(Pi/4)
      assert ( v(1, a) !=~ v(2, a) ) }
    
    "It does not equal a polar vector with a different angle" in
    { val m = 2.squareRoot
      assert ( v(m, Angle(Pi/4)) !=~ v(m, Angle(Pi/3)) ) }
    
    "It calculates its X value." in
    { assert ( v(2.squareRoot, Angle(Pi/4)).x =~ 1 ) }
    
    "It calculates its Y value." in
    { assert ( v(2.squareRoot, Angle(Pi/4)).y =~ 1 ) }
    
    "It can be negated." in
    { val m = 2.squareRoot
      assert ( -v(m, Angle(Pi/4)) =~ v(m, Angle(-3*Pi/4)) ) }
  }
}
