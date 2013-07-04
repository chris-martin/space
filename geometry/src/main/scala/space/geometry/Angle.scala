package space.geometry

import math.Pi

trait Angle {

  def toRadians: Radians
  def toDegrees: Degrees

  /** An equivalent angle between -Pi and Pi radians.
    */
  def normalize: this.type
}

object Angle {

  val radiansPerDegree = Pi / 180
  val degreesPerRadian = 180 / Pi

}

case class Radians(x: Double) extends Angle {

  override def toRadians: Radians = this
  override def toDegrees: Degrees = Degrees(x * Angle.degreesPerRadian)

  override def normalize = ???

}

/**
 * @param x radian value in the range [ -180, 180 ]
 */
case class Degrees(x: Double) extends Angle {

  override def toRadians: Radians = Radians(x * Angle.radiansPerDegree)
  override def toDegrees: Degrees = this

  override def normalize = ???

}
