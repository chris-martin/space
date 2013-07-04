package space.geometry.dimension2

import math.Pi

sealed trait Angular[A <: Angular[A]] extends Any {

  def toDouble: Double

  def sine: Double = math.sin(toDouble)
  def cosine: Double = math.cos(toDouble)

  protected def companion: AngularCompanion[A]

  def +(that: A): A = companion(toDouble + that.toDouble)
  def -(that: A): A = companion(toDouble - that.toDouble)

  def *(s: Double): A = companion(toDouble * s)
  def /(s: Double): A = companion(toDouble / s)

}

sealed trait AngularCompanion[T] {

  def apply(a: Double): T

  def halfCircle: T = apply(Pi)

}

class Radians (val toDouble: Double) extends AnyVal with Angular[Radians] {
  override protected def companion: AngularCompanion[Radians] = Radians
}

object Radians extends AngularCompanion[Radians] {
  override def apply(a: Double): Radians = new Radians(a)
}

/**
 * @param toDouble in the range (-Pi, Pi]
 */
class Angle private (val toDouble: Double) extends AnyVal with Angular[Angle] {
  override protected def companion: AngularCompanion[Angle] = Angle
}

object Angle extends AngularCompanion[Angle] {

  import math.Pi

  val twoPi = 2 * Pi

  override def apply(a: Double): Angle = new Angle(
    a % twoPi match {
      case b if b >   Pi => b - twoPi
      case b if b <= -Pi => b + twoPi
      case b             => b
    }
  )

  def apply(x: Double, y: Double): Angle = new Angle(math.atan2(y, x))

}
