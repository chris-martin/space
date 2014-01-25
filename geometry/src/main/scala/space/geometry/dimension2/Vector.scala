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

  /** Cross product.
    */
  def cross(that: Vector): Double =
    this ⋅ that.rotate(RightAngle.quarterCircle)

  def ×(that: Vector): Double = this cross that

  /** Scalar (dot) product.
    */
  def dot(that: Vector): Double = x * that.x + y * that.y

  def ⋅(that: Vector): Double = this dot that

  /** Rotation about the origin.
    */
  def rotate(a: AnyRadians): Vector

  /** Rotation about the origin.
    */
  def rotate(a: RightAngle): Vector

  /** Rotation about a pivot.
    */
  def rotate(a: AnyRadians, pivot: Vector): Vector

  /** Rotation about a pivot.
    */
  def rotate(a: RightAngle, pivot: Vector): Vector

  def reflect(pivot: LineLike): Vector =
    this + 2 * (this → pivot).difference
}

/** A two-dimensional vector defined by X and Y coordinates.
  */
sealed case class CartesianVector(x: Double, y: Double) extends Vector {

  override def toCartesian: CartesianVector = this

  override def magnitude: Double = sqrt(x.square + y.square)

  override def angle: CircleRadians = CircleRadians(x=x, y=y)

  override def unary_- : CartesianVector = CartesianVector ( -x, -y )

  override def +(that: Vector): CartesianVector =
    CartesianVector ( x + that.x, y + that.y )

  override def -(that: Vector): CartesianVector =
    CartesianVector ( x - that.x, y - that.y )

  override def *(s: Double): CartesianVector = new CartesianVector ( x*s, y*s )

  override def /(s: Double): CartesianVector = new CartesianVector ( x/s, y/s )

  override def rotate(a: AnyRadians): PolarVector = toPolar rotate a

  override def rotate(a: RightAngle): CartesianVector =
    a match {
      case RightAngle.quarterCircle => xy(-y, x)
      case RightAngle.halfCircle => xy(-x, -y)
      case RightAngle.threeQuarterCircle => xy(y, -x)
      case RightAngle.zero => this
    }

  override def rotate(a: AnyRadians, pivot: Vector): Vector =
    (this - pivot).rotate(a) + pivot

  override def rotate(a: RightAngle, pivot: Vector): CartesianVector =
    (this - pivot).rotate(a) + pivot
}

case class PolarVector(magnitude: Double, angle: CircleRadians)
    extends Vector {

  override def toPolar: PolarVector = this

  override def x: Double = magnitude * angle.cosine

  override def y: Double = magnitude * angle.sine

  override def unary_- : PolarVector =
    PolarVector(magnitude, angle + Radians.halfCircle)

  override def +(that: Vector): CartesianVector = toCartesian + that

  override def -(that: Vector): CartesianVector = toCartesian - that

  override def *(s: Double): PolarVector = PolarVector(magnitude * s, angle)

  override def /(s: Double): PolarVector = PolarVector(magnitude / s, angle)

  override def rotate(a: AnyRadians): PolarVector =
    PolarVector(magnitude, angle + a)

  override def rotate(a: AnyRadians, pivot: Vector): Vector =
    toCartesian.rotate(a, pivot)

  override def rotate(a: RightAngle): PolarVector = rotate(a.toRadians)

  override def rotate(a: RightAngle, pivot: Vector) =
    rotate(a.toRadians, pivot)
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

  override def rotate(a: AnyRadians): this.type = this

  override def rotate(a: AnyRadians, pivot: Vector): Vector =
    toCartesian.rotate(a, pivot)

  override def rotate(a: RightAngle): this.type = this

  override def rotate(a: RightAngle, pivot: Vector) =
    toCartesian.rotate(a, pivot)
}
