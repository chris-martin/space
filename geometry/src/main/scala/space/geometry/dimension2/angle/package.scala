package space.geometry
package dimension2

/**
 * Implicit conversions between angle types are defined only when the conversion imposes a restriction
 * on the angle's range. Conversions that lift a restriction are not done implicitly, because the result
 * would be ambiguous (for example, Pi/4 semicircle radians could be converted to either  Pi/4 or -Pi/4
 * circle radians, so there is no implicit semicircle-to-circle conversion).
 */
package object angle {

  private[angle] val Pi = math.Pi
  private[angle] val twoPi = 2 * Pi

  implicit class Dimension2Double(x: Double) {
    def radians: ArbitraryRadians = ArbitraryRadians(x)
  }

  // implicit conversions between angle types

  implicit def circleToSemicircle(x: CircleRadians): SemicircleRadians =
    SemicircleRadians(x.toDouble)

  implicit def arbitraryToCircle(x: ArbitraryRadians): CircleRadians =
    CircleRadians(x.toDouble)

  implicit def arbitraryToSemicircle(x: ArbitraryRadians): SemicircleRadians =
    SemicircleRadians(x.toDouble)

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
