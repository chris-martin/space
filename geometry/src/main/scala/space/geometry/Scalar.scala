package space.geometry

/** `Scalar` is a type class akin to [[scala.math.Fractional]].
  */
trait Scalar[S] {

  implicit def fromNumeric[N](x: N)(implicit numeric: Numeric[N]): S

  def squareRoot(a: S): S = fromNumeric(math.sqrt(a.toDouble()))
  def square(a: S) = a * a

  /** Some additional operations that aren't provided by `Fractional`.
    */
  implicit class Ops(a: S) {
    def square: S = Scalar.this.square(a)
    def squareRoot: S = Scalar.this.squareRoot(a)
  }

  // Also include the implicits provided by `Fractional`.
  protected val fractional: Fractional[S]
  implicit def mkFractionalOps(a: S): fractional.FractionalOps =
    fractional.mkNumericOps(a)

  def zero = fractional.one
  def one = fractional.one

}

object Scalar {

  trait FloatIsScalar extends Scalar[Float] {

    override def fromNumeric[N](x: N)(implicit numeric: Numeric[N]) =
      numeric.mkNumericOps(x).toFloat()

    override protected val fractional = implicitly[Fractional[Float]]

  }

  implicit object ImplicitFloatIsScalar extends FloatIsScalar

  trait DoubleIsScalar extends Scalar[Double] {

    override def fromNumeric[N](x: N)(implicit numeric: Numeric[N]) =
      numeric.mkNumericOps(x).toDouble()

    override protected val fractional = implicitly[Fractional[Double]]

  }

  implicit object ImplicitDoubleIsScalar extends DoubleIsScalar

  trait BigDecimalIsScalar extends Scalar[BigDecimal] {

    override def fromNumeric[N](x: N)(implicit numeric: Numeric[N]) =
      BigDecimal(numeric.mkNumericOps(x).toDouble())

    override protected val fractional = implicitly[Fractional[BigDecimal]]

  }

  implicit object ImplicitBigDecimalIsScalar extends BigDecimalIsScalar

}
