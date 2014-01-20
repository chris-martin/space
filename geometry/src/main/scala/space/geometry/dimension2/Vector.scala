package space.geometry
package dimension2

/** A point in the Euclidean plane.
  */
trait Vector {

  def x: Double

  def y: Double

  def toCartesian: CartesianVector = CartesianVector(x, y)

  def toPolar: PolarVector = PolarVector(magnitude, angle)

  def magnitude: Double

  def angle: CircleRadians

  def unary_- : Vector

  def +(that: Vector): Vector

  def -(that: Vector): Vector

  def *(s: Double): Vector

  def /(s: Double): Vector

  def rotate(a: ArbitraryRadians): Vector
}

/** A two-dimensional vector defined by X and Y coordinates.
  */
sealed case class CartesianVector(x: Double, y: Double) extends Vector {

  override def toCartesian: CartesianVector = this

  override def magnitude: Double = (x.square + y.square).squareRoot

  override def angle: CircleRadians = CircleRadians(x=x, y=y)

  override def unary_- : CartesianVector = CartesianVector ( -x, -y )

  override def +(that: Vector): CartesianVector =
  CartesianVector ( x + that.x, y + that.y )

  override def -(that: Vector): CartesianVector =
  CartesianVector ( x - that.x, y - that.y )

  override def *(s: Double): CartesianVector = new CartesianVector ( x*s, y*s )

  override def /(s: Double): CartesianVector = new CartesianVector ( x/s, y/s )

  override def rotate(a: ArbitraryRadians): PolarVector = toPolar.rotate(a)
}

case class PolarVector(magnitude: Double, angle: CircleRadians) extends
Vector {

  override def toPolar: PolarVector = this

  override def x: Double = magnitude * angle.cosine

  override def y: Double = magnitude * angle.sine

  override def unary_- : PolarVector =
  PolarVector(magnitude, angle + Angle.halfCircle)

  override def +(that: Vector): CartesianVector = toCartesian + that

  override def -(that: Vector): CartesianVector = toCartesian - that

  override def *(s: Double): PolarVector = PolarVector(magnitude * s, angle)

  override def /(s: Double): PolarVector = PolarVector(magnitude / s, angle)

  override def rotate(a: ArbitraryRadians): PolarVector =
  PolarVector(magnitude, angle + a)
}

object Origin extends Vector {

  override def x: Double = 0

  override def y: Double = 0

  override val toCartesian: CartesianVector = CartesianVector(x = 0, y = 0)

  override val toPolar: PolarVector =
  PolarVector(magnitude = 0, angle = 0.radians)

  override def magnitude: Double = 0

  override def angle: CircleRadians = 0.radians

  override def unary_- : this.type = this

  override def +(that: Vector): Vector = that

  override def -(that: Vector): Vector = -that

  override def *(s: Double): this.type = this

  override def /(s: Double): this.type = this

  override def rotate(a: ArbitraryRadians): this.type = this
}
