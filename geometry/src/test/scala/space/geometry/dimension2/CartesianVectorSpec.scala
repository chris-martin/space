package space.geometry.dimension2

import org.scalatest._

class CartesianVectorSpec extends FunSpec {

  describe("CartesianVector[Double](3, 4)") {

    def v(x: Double, y: Double): CartesianVector[Double] = CartesianVector(x, y)

    val a = v(3, 4)

    it ("equals (3, 4)") { assert ( a == v(3, 4) ) }
    it ("does not equal (30, 4)") { assert ( a != v(30, 4) ) }
    it ("does not equal (3, 40)") { assert ( a != v(3, 40) ) }

    it ("magnitude equals 5") { assert ( a.magnitude === 5 ) }
    it ("negated equals (-3, -4)") { assert ( -a === v(-3, -4) ) }
    it ("plus (1, 3) equals (4, 7)") { assert ( a + v(1, 3) === v(4, 7) ) }
    it ("minus (1, 3) equals (2, 1)") { assert ( a - v(1, 3) === v(2, 1) ) }
    it ("times 1.5 equals (4.5, 6)") { assert ( a * 1.5 === v(4.5, 6) ) }
    it ("divided by 2 equals (1.5, 2)") { assert ( v(3, 4) / 2 === v(1.5, 2) ) }
  }

}
