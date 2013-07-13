package space.geometry
package dimension2

/** @param toDouble in the range [0, Pi)
  */
class SemicircleRadians private (val toDouble: Double)
    extends AnyVal with Angle[SemicircleRadians] {

  override protected def companion: AngleCompanion[SemicircleRadians] = SemicircleRadians

  override def toString = "SemicircleRadians(%f)" format toDouble

}

object SemicircleRadians extends AngleCompanion[SemicircleRadians] {

  override def apply(a: Double): SemicircleRadians =
    new SemicircleRadians(
      a % Pi match {
        case b  if b < 0  => b + Pi
        case b            => b
      }
    )

  def apply(x: Double, y: Double): SemicircleRadians =
    SemicircleRadians(math.atan2(y, x))

}
