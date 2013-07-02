package space.geometry

import math.Pi

trait Angle[S] extends ImplicitScalar[S] {

  def toRadians: Radians[S]
  def toDegrees: Degrees[S]

  /** An equivalent angle between -Pi and Pi radians.
    */
  def normalize: this.type
}

object Angle {

  val radiansPerDegree = Pi / 180
  val degreesPerRadian = 180 / Pi

}

case class Radians[S](x: S)(implicit val scalar: Scalar[S]) extends Angle[S] {

  import scalar._

  override def toRadians: Radians[S] = this
  override def toDegrees: Degrees[S] = Degrees(x * Angle.degreesPerRadian)

  override def normalize = ???

}

/**
 * @param x radian value in the range [ -180, 180 ]
 */
case class Degrees[S](x: S)(implicit val scalar: Scalar[S]) extends Angle[S] {

  import scalar._

  override def toRadians: Radians[S] = Radians(x * Angle.radiansPerDegree)
  override def toDegrees: Degrees[S] = this

  override def normalize = ???

}
