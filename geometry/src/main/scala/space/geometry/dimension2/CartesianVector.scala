package space.geometry
package dimension2

/** A two-dimensional vector defined by X and Y coordinates.
  */
sealed case class CartesianVector(x: Double, y: Double) extends Vector {

  override def toCartesian: CartesianVector = this
  override def toPolar: PolarVector = PolarVector(magnitude, angle)

  override def magnitude: Double = (x.square + y.square).squareRoot
  override def angle: CircleRadians = CircleRadians(x=x, y=y)

  override def unary_- : CartesianVector = CartesianVector ( -x, -y )

  override def +(that: Vector): CartesianVector = CartesianVector ( x + that.x, y + that.y )
  override def -(that: Vector): CartesianVector = CartesianVector ( x - that.x, y - that.y )

  override def *(s: Double): CartesianVector = new CartesianVector ( x*s, y*s )
  override def /(s: Double): CartesianVector = new CartesianVector ( x/s, y/s )

  override def rotate(a: CircleRadians): PolarVector = toPolar.rotate(a)

}
