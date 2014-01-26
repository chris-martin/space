package space.geometry
package dimension2

/** A point on the Euclidean plane.
  */
trait Point {

  def x: Double

  def y: Double

  def toCartesian: CartesianVector = CartesianVector(x, y)

  def toPolar: PolarVector = PolarVector(magnitude, angle)

  def magnitude: Double

  def angle: CircleRadians

  def unary_- : Point

  def +(that: Point): Point

  def -(that: Point): Point

  def *(s: Double): Point

  def /(s: Double): Point

  /** Cross product.
    */
  def cross(that: Point): Double =
    this ⋅ that.rotate(RightAngle.quarterCircle)

  def ×(that: Point): Double = this cross that

  /** Scalar (dot) product.
    */
  def dot(that: Point): Double = x * that.x + y * that.y

  def ⋅(that: Point): Double = this dot that

  /** Rotation about the origin.
    */
  def rotate(a: AnyRadians): Point

  /** Rotation about the origin.
    */
  def rotate(a: RightAngle): Point

  /** Rotation about a pivot.
    */
  def rotate(a: AnyRadians, pivot: Point): Point

  /** Rotation about a pivot.
    */
  def rotate(a: RightAngle, pivot: Point): Point

  def reflect(pivot: LineLike): Point =
    this + 2 * (this → pivot).difference
}

/** A point on the Euclidean plane defined by `x` and `y` coordinates.
  */
sealed case class CartesianVector(x: Double, y: Double) extends Point {

  override def toCartesian: CartesianVector = this

  override def magnitude: Double = sqrt(x.square + y.square)

  override def angle: CircleRadians = CircleRadians(x=x, y=y)

  override def unary_- : CartesianVector = CartesianVector ( -x, -y )

  override def +(that: Point): CartesianVector =
    CartesianVector ( x + that.x, y + that.y )

  override def -(that: Point): CartesianVector =
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

  override def rotate(a: AnyRadians, pivot: Point): Point =
    (this - pivot).rotate(a) + pivot

  override def rotate(a: RightAngle, pivot: Point): CartesianVector =
    (this - pivot).rotate(a) + pivot
}

case class PolarVector(magnitude: Double, angle: CircleRadians)
    extends Point {

  override def toPolar: PolarVector = this

  override def x: Double = magnitude * angle.cosine

  override def y: Double = magnitude * angle.sine

  override def unary_- : PolarVector =
    PolarVector(magnitude, angle + Radians.halfCircle)

  override def +(that: Point): CartesianVector = toCartesian + that

  override def -(that: Point): CartesianVector = toCartesian - that

  override def *(s: Double): PolarVector = PolarVector(magnitude * s, angle)

  override def /(s: Double): PolarVector = PolarVector(magnitude / s, angle)

  override def rotate(a: AnyRadians): PolarVector =
    PolarVector(magnitude, angle + a)

  override def rotate(a: AnyRadians, pivot: Point): Point =
    toCartesian.rotate(a, pivot)

  override def rotate(a: RightAngle): PolarVector = rotate(a.toRadians)

  override def rotate(a: RightAngle, pivot: Point) =
    rotate(a.toRadians, pivot)
}

object Origin extends Point {

  override def x: Double = 0

  override def y: Double = 0

  override val toCartesian: CartesianVector = CartesianVector(x = 0, y = 0)

  override val toPolar: PolarVector =
    PolarVector(magnitude = 0, angle = 0.radians)

  override def magnitude: Double = 0

  override def angle: CircleRadians = 0.radians

  override def unary_- : this.type = this

  override def +(that: Point): Point = that

  override def -(that: Point): Point = -that

  override def *(s: Double): this.type = this

  override def /(s: Double): this.type = this

  override def rotate(a: AnyRadians): this.type = this

  override def rotate(a: AnyRadians, pivot: Point): Point =
    toCartesian.rotate(a, pivot)

  override def rotate(a: RightAngle): this.type = this

  override def rotate(a: RightAngle, pivot: Point) =
    toCartesian.rotate(a, pivot)
}
