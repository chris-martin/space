package space

package object geometry {

  val Pi = math.Pi

  val twoPi = 2 * Pi

  implicit class DoubleEnrichedForGeometry(val x: Double) extends AnyVal {

    def square: Double = x * x

    def squareRoot: Double = math.sqrt(x)
  }

  implicit val DoubleApproximation = space.approximation.DoubleApproximation
}
