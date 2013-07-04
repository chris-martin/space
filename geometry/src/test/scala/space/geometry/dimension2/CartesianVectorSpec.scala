package space.geometry
package dimension2

import org.scalatest._

class CartesianVectorSpec extends FunSpec {

  describe("A CartesianVector") {

    val v = CartesianVector(_, _)
    val a = v(3, 4)

    it ("equals an identical cartesian vector") { assert ( a == v(3, 4) ) }
    it ("does not equal a vector with a different X") { assert ( a != v(30, 4) ) }
    it ("does not equal a vector with a different Y") { assert ( a != v(3, 40) ) }
    it ("calculates its magnitude") { assert ( a.magnitude === 5 ) }
    it ("can be negated") { assert ( -a === v(-3, -4) ) }
    it ("can be added to another cartesian vector") { assert ( a + v(1, 3) === v(4, 7) ) }
    it ("can be subtracted from another cartesian vector") { assert ( a - v(1, 3) === v(2, 1) ) }
    it ("can have its magnitude multiplied by a scalar") { assert ( a * 1.5 === v(4.5, 6) ) }
    it ("can have its magnitude divided by a scalar") { assert ( v(3, 4) / 2 === v(1.5, 2) ) }
  }

}
