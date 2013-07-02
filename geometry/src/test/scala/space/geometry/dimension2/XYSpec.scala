package space.geometry.dimension2

import org.scalatest._

class XYSpec extends FunSpec {

  /** Most of the XY tests are for BigDecimal rather than Float or Double to
    * avoid complications with round-off error from floating-point arithmetic.
    */
  describe("XY[BigDecimal](3, 4)") {

    /** Parses a whitespace-delimited 2-tuple of decimal numbers as an `XY`.
      */
    def $(s: String): XY[BigDecimal] = {
      val xy = s.split("""\s+""").map(_.trim).map(BigDecimal(_))
      XY(xy(0), xy(1))
    }

    val a = $("3 4")

    it ("equals (3, 4)")          { assert ( a == $("3 4") ) }
    it ("does not equal (30, 4)") { assert ( a != $("30 4") ) }
    it ("does not equal (3, 40)") { assert ( a != $("3 40") ) }

    it ("magnitude equals 5")           { assert ( a.magnitude === 5 ) }
    it ("negated equals (-3, -4)")      { assert ( -a === $("-3 -4") ) }
    it ("plus (1, 3) equals (4, 7)")    { assert ( a + $("1 3") === $("4 7") ) }
    it ("minus (1, 3) equals (2, 1)")   { assert ( a - $("1 3") === $("2 1") ) }
    it ("times 1.5 equals (4.5, 6)")    { assert ( a * 1.5 === $("4.5  6") ) }
    it ("divided by 2 equals (1.5, 2)") { assert ( $("3 4") / 2 === $("1.5  2") ) }

    it ("cannot be divided by 0") { intercept[ArithmeticException] ( $("3 4") / 0 ) }
  }

  describe("XY addition") {

    it ("works with Floats")  {
      def F = XY[Float](_, _)
      assert ( F(3, 4) + F(30, 40) === F(33, 44) )
    }

    it ("works with Doubles") {
      def D = XY[Float](_, _)
      assert ( D(3, 4) + D(30, 40) === D(33, 44) )
    }
  }

}
