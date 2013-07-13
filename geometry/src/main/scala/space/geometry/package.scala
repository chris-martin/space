package space

package object geometry {

  val Pi = math.Pi
  val twoPi = 2 * Pi

  implicit class GeometryDouble(val x: Double) extends AnyVal {

    def square: Double = x * x

    def squareRoot: Double = math.sqrt(x)

  }

  implicit object ScalarDistance extends ScalarDifference[Double] {
    override def distance(a: Double, b: Double) = (a-b).abs
  }

}
