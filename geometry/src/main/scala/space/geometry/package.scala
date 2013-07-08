package space

package object geometry {

  implicit class RichDouble(val x: Double) extends AnyVal {

    def square: Double = x * x

    def squareRoot: Double = math.sqrt(x)

  }

  implicit object ScalarDistance extends Distance[Double] {
    override def distance(a: Double, b: Double) = (a-b).abs
  }

}
