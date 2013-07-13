package space.geometry
package dimension2

trait Angle[A <: Angle[A]] extends Any {

  def toDouble: Double

  def sine: Double = math.sin(toDouble)
  def cosine: Double = math.cos(toDouble)

  protected def companion: AngleCompanion[A]

  def +(that: A): A = companion(toDouble + that.toDouble)
  def -(that: A): A = companion(toDouble - that.toDouble)

  def *(s: Double): A = companion(toDouble * s)
  def /(s: Double): A = companion(toDouble / s)

}

object Angle extends ArbitraryRadiansCompanion

trait AngleCompanion[A] {

  def apply(a: Double): A

  def halfCircle: A = apply(Pi)
  def circle: A = apply(twoPi)

}

class ArbitraryRadians (val toDouble: Double)
  extends AnyVal with Angle[ArbitraryRadians] {

  override protected def companion: AngleCompanion[ArbitraryRadians] = ArbitraryRadians

  override def toString = "ArbitraryRadians(%f)" format toDouble

}

trait ArbitraryRadiansCompanion extends AngleCompanion[ArbitraryRadians] {

  override def apply(a: Double): ArbitraryRadians = new ArbitraryRadians(a)

}

object ArbitraryRadians extends ArbitraryRadiansCompanion

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
