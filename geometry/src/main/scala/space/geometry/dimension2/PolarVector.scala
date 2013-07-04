package space.geometry
package dimension2

case class PolarVector(magnitude: Double, angle: Angle) extends Vector {

  def toPolar = this
  def toCartesian: CartesianVector = CartesianVector(x, y)

  def x = magnitude * angle.cosine
  def y = magnitude * angle.sine

  def unary_- : PolarVector = PolarVector(magnitude, angle + Angle.halfCircle)

  def +(that: Vector): CartesianVector = toCartesian + that
  def -(that: Vector): CartesianVector = toCartesian - that

  def *(s: Double) = PolarVector(magnitude * s, angle)
  def /(s: Double) = PolarVector(magnitude / s, angle)

  def rotate(a: Angular) = PolarVector(magnitude, angle + a)

}
