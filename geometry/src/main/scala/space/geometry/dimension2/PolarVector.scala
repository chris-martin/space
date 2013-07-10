package space.geometry
package dimension2

import angle._

case class PolarVector(magnitude: Double, angle: CircleRadians) extends Vector {

  override def toPolar: PolarVector = this
  override def toCartesian: CartesianVector = CartesianVector(x, y)

  override def x: Double = magnitude * angle.cosine
  override def y: Double = magnitude * angle.sine

  override def unary_- : PolarVector = PolarVector(magnitude, angle + Angle.halfCircle)

  override def +(that: Vector): CartesianVector = toCartesian + that
  override def -(that: Vector): CartesianVector = toCartesian - that

  override def *(s: Double): PolarVector = PolarVector(magnitude * s, angle)
  override def /(s: Double): PolarVector = PolarVector(magnitude / s, angle)

  override def rotate(a: CircleRadians): PolarVector = PolarVector(magnitude, angle + a)

}
