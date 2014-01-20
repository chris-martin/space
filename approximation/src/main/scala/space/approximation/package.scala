package space

package object approximation {

  implicit object DoubleApproximation extends Approximation[Double] {

    override def apply(left: Double, right: Double)(implicit tolerance:
    Tolerance): Boolean = (left-right).abs < tolerance
  }
}
