package space.geometry

/** `IsScalar` is a type class akin to [[scala.math.Fractional]].
  */
trait IsScalar[Scalar] {

  implicit def fromNumeric[Number](x: Number)(implicit numeric: Numeric[Number]): Scalar

  def squareRoot(a: Scalar): Scalar = fromNumeric(math.sqrt(a.toDouble()))
  def square(a: Scalar) = a * a

  /** Some additional operations that aren't provided by `Fractional`.
    */
  implicit class Ops(a: Scalar) {
    def square: Scalar = IsScalar.this.square(a)
    def squareRoot: Scalar = IsScalar.this.squareRoot(a)
  }

  // Also include the implicits provided by `Fractional`.
  protected val fractional: Fractional[Scalar]
  implicit def mkFractionalOps(a: Scalar): fractional.FractionalOps =
    fractional.mkNumericOps(a)

}

object IsScalar {

  trait FloatIsScalar extends IsScalar[Float] {

    override def fromNumeric[Number](x: Number)(implicit numeric: Numeric[Number]) =
      numeric.mkNumericOps(x).toFloat()

    override protected val fractional = implicitly[Fractional[Float]]

  }

  implicit object ImplicitFloatIsScalar extends FloatIsScalar

  trait DoubleIsScalar extends IsScalar[Double] {

    override def fromNumeric[Number](x: Number)(implicit numeric: Numeric[Number]) =
      numeric.mkNumericOps(x).toDouble()

    override protected val fractional = implicitly[Fractional[Double]]

  }

  implicit object ImplicitDoubleIsScalar extends DoubleIsScalar

  trait BigDecimalIsScalar extends IsScalar[BigDecimal] {

    override def fromNumeric[Number](x: Number)(implicit numeric: Numeric[Number]) =
      BigDecimal(numeric.mkNumericOps(x).toDouble())

    override protected val fractional = implicitly[Fractional[BigDecimal]]

  }

  implicit object ImplicitBigDecimalIsScalar extends BigDecimalIsScalar

}
