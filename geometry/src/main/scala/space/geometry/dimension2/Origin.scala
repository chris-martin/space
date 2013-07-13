package space.geometry
package dimension2

object Origin extends Vector {

  override def x: Double = 0
  override def y: Double = 0

  override def toCartesian: CartesianVector = CartesianVector(0, 0)
  override def toPolar: PolarVector = PolarVector(0, CircleRadians(0))

  override def magnitude: Double = 0
  override def angle = Angle(0)

  override def unary_- : this.type = this

  override def +(that: Vector): Vector = that
  override def -(that: Vector): Vector = -that

  override def *(s: Double): this.type = this
  override def /(s: Double): this.type = this

  override def rotate(a: CircleRadians): this.type = this

}
