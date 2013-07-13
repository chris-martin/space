package space.geometry

package object dimension2 {

  implicit object VectorDistance extends ScalarDifference[Vector] {

    override def distance(a: Vector, b: Vector) =
      (a - b).magnitude

  }

  implicit class Dimension2Double(x: Double) {
    def radians: ArbitraryRadians = ArbitraryRadians(x)
  }

  // scalar differences for each angle type

  implicit object ArbitraryRadiansDistance extends ScalarDifference[ArbitraryRadians] {

    override def distance(a: ArbitraryRadians, b: ArbitraryRadians) = (a.toDouble - b.toDouble).abs

  }

  implicit object CircleRadiansDistance extends ScalarDifference[CircleRadians] {

    override def distance(a: CircleRadians, b: CircleRadians) =
      CircleRadians(a.toDouble - b.toDouble).toDouble.abs

  }

  implicit object SemicircleRadiansDistance extends ScalarDifference[SemicircleRadians] {

    override def distance(a: SemicircleRadians, b: SemicircleRadians) =
      SemicircleRadians(a.toDouble - b.toDouble).toDouble.abs

  }

}
