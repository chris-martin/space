package space.geometry
package dimension2
package vector

import math.Pi
import angle._

class PolarVectorSpec extends org.scalatest.FunSpec with NearEqualityTesting {

  describe("A PolarVector") {

    val v = PolarVector(_, _)
    val a = v(2.squareRoot, CircleRadians(Pi/4))

    it ("equals an identical polar vector") { assert ( a =~ v(2.squareRoot, CircleRadians(Pi/4)) ) }
    it ("equals itself rotated by 2 Pi") { assert ( a =~ a.rotate(ArbitraryRadians(2*Pi)) ) }
    it ("does not equal a polar vector with a different magnitude") { assert ( a !=~ v(2, CircleRadians(Pi/4)) ) }
    it ("does not equal a polar vector with a different angle") { assert ( a !=~ v(2.squareRoot, CircleRadians(Pi/3)) ) }

    it ("calculates its X value") { assert ( a.x =~ 1 ) }
    it ("calculates its Y value") { assert ( a.y =~ 1 ) }
    it ("can be negated") { assert ( -a =~ v(2.squareRoot, CircleRadians(-3*Pi/4)) ) }
  }

}
