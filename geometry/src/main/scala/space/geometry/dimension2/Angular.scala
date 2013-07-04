package space.geometry
package dimension2

import math.Pi

sealed trait Angular extends Any {

  def toDouble: Double

  def sine: Double = math.sin(toDouble)
  def cosine: Double = math.cos(toDouble)

}

sealed trait TypedAngular[A] extends Any {

  def toDouble: Double

  protected def companion: AngularCompanion[A]

  def +(that: Angular): A = companion(toDouble + that.toDouble)
  def -(that: Angular): A = companion(toDouble - that.toDouble)

  def *(s: Double): A = companion(toDouble * s)
  def /(s: Double): A = companion(toDouble / s)

}

sealed trait AngularCompanion[T] {

  def apply(a: Double): T

  def halfCircle: T = apply(Pi)

}

class Radians (val toDouble: Double) extends AnyVal with Angular with TypedAngular[Radians] {
  override protected def companion: AngularCompanion[Radians] = Radians
}

object Radians extends AngularCompanion[Radians] {
  override def apply(a: Double): Radians = new Radians(a)
}

/**
 * @param toDouble in the range (-Pi, Pi]
 */
class Angle private (val toDouble: Double) extends AnyVal with Angular with TypedAngular[Angle] {
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
