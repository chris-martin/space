package space

package object geometry {

  val Pi = math.Pi

  val twoPi = 2 * Pi

  val sqrt = math.sqrt _

  implicit class DoubleEnrichedForGeometry(val x: Double) extends AnyVal {

    def square: Double = x * x
  }

  implicit val DoubleApproximation = space.approximation.DoubleApproximation
}
