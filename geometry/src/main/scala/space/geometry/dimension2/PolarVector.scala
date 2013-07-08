package space.geometry
package dimension2

case class PolarVector(magnitude: Double, angle: Angle) extends Vector {

  override def toPolar = this
  override def toCartesian: CartesianVector = CartesianVector(x, y)

  override def x = magnitude * angle.cosine
  override def y = magnitude * angle.sine

  override def unary_- : PolarVector = PolarVector(magnitude, angle + Angle.halfCircle)

  override def +(that: Vector): CartesianVector = toCartesian + that
  override def -(that: Vector): CartesianVector = toCartesian - that

  override def *(s: Double) = PolarVector(magnitude * s, angle)
  override def /(s: Double) = PolarVector(magnitude / s, angle)

  override def rotate(a: Angular) = PolarVector(magnitude, angle + a)

}
