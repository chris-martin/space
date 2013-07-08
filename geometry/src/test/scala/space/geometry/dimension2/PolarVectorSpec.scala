package space.geometry
package dimension2

import org.scalatest._

import math.Pi

class PolarVectorSpec extends FunSpec with NearEqualityTesting {

  describe("A PolarVector") {

    val v = PolarVector(_, _)
    val a = v(2.squareRoot, Angle(Pi/4))

    it ("equals an identical polar vector") { assert ( a =~ v(2.squareRoot, Angle(Pi/4)) ) }
    it ("equals itself rotated by 2 Pi") { assert ( a =~ a.rotate(Radians(2*Pi)) ) }
    it ("does not equal a polar vector with a different magnitude") { assert ( a !=~ v(2, Angle(Pi/4)) ) }
    it ("does not equal a polar vector with a different angle") { assert ( a !=~ v(2.squareRoot, Angle(Pi/3)) ) }
    it ("calculates its X value") { assert ( a.x =~ 1 ) }
    it ("calculates its Y value") { assert ( a.y =~ 1 ) }
    it ("can be negated") { assert ( -a =~ v(2.squareRoot, Angle(-3*Pi/4)) ) }
  }

}
