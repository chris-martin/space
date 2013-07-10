package space.geometry
package dimension2
package angle

/** @param toDouble in the range [-Pi, Pi)
  */
class CircleRadians private (val toDouble: Double)
    extends AnyVal with Angle[CircleRadians] {

  override protected def companion: AngleCompanion[CircleRadians] = CircleRadians

  override def toString = "CircleRadians(%f)" format toDouble

}

object CircleRadians extends AngleCompanion[CircleRadians] {

  override def apply(a: Double): CircleRadians =
    new CircleRadians(
      a % twoPi match {
        case b  if b >= Pi  =>  b - twoPi
        case b  if b < -Pi  =>  b + twoPi
        case b              =>  b
      }
    )

  def apply(x: Double, y: Double): CircleRadians =
    new CircleRadians(math.atan2(y, x))

}
