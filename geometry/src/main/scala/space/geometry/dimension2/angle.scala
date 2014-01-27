package space.geometry
package dimension2

/** Note that implicit conversions between angle types are defined only when
  * the conversion imposes a restriction on the angle's range. Conversions
  * that lift a restriction are not done implicitly, because the result would
  * be ambiguous (for example, Pi/4 semicircle radians could be converted to
  * either  Pi/4 or -Pi/4 circle radians, so there is no implicit
  * semicircle-to-circle conversion).
  */
object Radians extends AnyRadiansCompanion {

  def apply(a: Point, b: Point, c: Point): SemicircleRadians = {
    val A = (b → c).length
    val B = (c → a).length
    val C = (a → b).length
    SemicircleRadians(math.acos((A*A + C*C - B*B) / (2*A*C)))
  }
}

trait GenRadians[A <: GenRadians[A]] extends Any {

  protected def companion: RadiansCompanion[A]

  def toDouble: Double

  def sine: Double = math.sin(toDouble)

  def cosine: Double = math.cos(toDouble)

  def +(that: A): A = companion(toDouble + that.toDouble)

  def -(that: A): A = companion(toDouble - that.toDouble)

  def *(s: Double): A = companion(toDouble * s)

  def /(s: Double): A = companion(toDouble / s)

  def unary_- : A = companion(-toDouble)
}

trait RadiansCompanion[A] {

  def apply(a: Double): A

  def quarterCircle: A = apply(Pi/2)

  def halfCircle: A = apply(Pi)

  def circle: A = apply(twoPi)
}

class AnyRadians (val toDouble: Double) extends AnyVal
    with GenRadians[AnyRadians] {

  override protected def companion: RadiansCompanion[AnyRadians] = AnyRadians

  override def toString = "AnyRadians($toDouble)"
}

trait AnyRadiansCompanion extends RadiansCompanion[AnyRadians] {

  override def apply(a: Double): AnyRadians = new AnyRadians(a)
}

object AnyRadians extends AnyRadiansCompanion {

  implicit def toCircle(x: AnyRadians): CircleRadians =
    CircleRadians(x.toDouble)

  implicit def toSemicircle(x: AnyRadians): SemicircleRadians =
    SemicircleRadians(x.toDouble)

  implicit def toQuarterCircle(x: AnyRadians): QuarterCircleRadians =
    QuarterCircleRadians(x.toDouble)
}

/** @param toDouble in the range [-Pi, Pi)
  */
class CircleRadians private (val toDouble: Double)
    extends AnyVal with GenRadians[CircleRadians] {

  def sign: Sign = if (toDouble > 0) Positive else Negative

  def toAnyRadiansArbitrarily: AnyRadians = AnyRadians(toDouble)

  override protected def companion: RadiansCompanion[CircleRadians] =
    CircleRadians

  override def toString = s"CircleRadians($toDouble)"
}

object CircleRadians extends RadiansCompanion[CircleRadians] {

  override def apply(a: Double): CircleRadians = new CircleRadians({
    val b = a % twoPi
    if (b >= Pi) b - twoPi else if (b < -Pi) b + twoPi else b
  })

  def apply(x: Double, y: Double): CircleRadians =
    new CircleRadians(math.atan2(y, x))

  implicit def toSemicircle(x: CircleRadians): SemicircleRadians =
    SemicircleRadians(x.toDouble)

  implicit def toQuarterCircle(x: CircleRadians): QuarterCircleRadians =
    QuarterCircleRadians(x.toDouble)
}

/** @param toDouble in the range [0, Pi)
  */
class SemicircleRadians private (val toDouble: Double) extends AnyVal
    with GenRadians[SemicircleRadians] {

  def orthogonal: SemicircleRadians = this + Radians(Pi/2)

  def toCircleRadians(sign: Sign): CircleRadians =
    CircleRadians(sign match {
      case Positive => toDouble
      case Negative => toDouble - Pi
    })

  def toCircleRadiansArbitrarily: CircleRadians = CircleRadians(toDouble)

  def toAnyRadiansArbitrarily: AnyRadians = AnyRadians(toDouble)

  override protected def companion: RadiansCompanion[SemicircleRadians] =
    SemicircleRadians

  override def toString = s"SemicircleRadians($toDouble)"
}

object SemicircleRadians extends RadiansCompanion[SemicircleRadians] {

  override def apply(a: Double): SemicircleRadians =
    new SemicircleRadians({
      val b = a % Pi
      if (b < 0) b + Pi else b
    })

  def apply(x: Double, y: Double): SemicircleRadians =
    SemicircleRadians(math.atan2(y, x))

  implicit def toQuarterCircle(x: SemicircleRadians): QuarterCircleRadians =
    QuarterCircleRadians(x.toDouble)
}

/** @param toDouble in the range [0, Pi/2)
  */
class QuarterCircleRadians private (val toDouble: Double) extends AnyVal
with GenRadians[QuarterCircleRadians] {

  def toSemicircleRadiansArbitrarily: SemicircleRadians =
    SemicircleRadians(toDouble)

  def toCircleRadiansArbitrarily: CircleRadians = CircleRadians(toDouble)

  def toAnyRadiansArbitrarily: AnyRadians = AnyRadians(toDouble)

  override protected def companion: RadiansCompanion[QuarterCircleRadians] =
    QuarterCircleRadians

  override def toString = s"QuarterCircleRadians($toDouble)"
}

object QuarterCircleRadians extends RadiansCompanion[QuarterCircleRadians] {

  override def apply(a: Double): QuarterCircleRadians =
    new QuarterCircleRadians({
      val b = a % (Pi/2)
      if (b < 0) b + (Pi/2) else b
    })
}

/** An angle that is a multiple of Pi/2.
  */
sealed trait RightAngle {

  def toRadians: AnyRadians

  def unary_- : RightAngle
}

object RightAngle {

  object zero extends RightAngle {
    override val toRadians: AnyRadians = Radians(0)
    override def unary_- : RightAngle = this
  }

  object quarterCircle extends RightAngle {
    override val toRadians: AnyRadians = Radians(Pi/2)
    override def unary_- : RightAngle = threeQuarterCircle
  }

  object halfCircle extends RightAngle {
    override val toRadians: AnyRadians = Radians(Pi)
    override def unary_- : RightAngle = this
  }

  object threeQuarterCircle extends RightAngle {
    override val toRadians: AnyRadians = Radians(3*Pi/2)
    override def unary_- : RightAngle = quarterCircle
  }
}
