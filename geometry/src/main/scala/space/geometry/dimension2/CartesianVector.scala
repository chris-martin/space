package space.geometry
package dimension2

/** A two-dimensional vector defined by X and Y coordinates.
  */
sealed class CartesianVector[Scalar: IsScalar](
  val x: Scalar, val y: Scalar,
  private var magnitudeOption: Option[Scalar] = None
) extends Vector[Scalar] {

  private val isScalar = implicitly[IsScalar[Scalar]]
  import isScalar._

  def xy: CartesianVector[Scalar] = this

  /** This result is memoized (its computation cost is incurred at most once).
    */
  override def magnitude: Scalar = magnitudeOption.getOrElse {
    (x.square + y.square).squareRoot K { m => magnitudeOption = Some(m) }
  }

  override def unary_- : CartesianVector[Scalar] = CartesianVector ( -x, -y )

  override def +(that: Vector[Scalar]) = CartesianVector ( x + that.x, y + that.y )
  override def -(that: Vector[Scalar]) = CartesianVector ( x - that.x, y - that.y )

  def *(s: Scalar): CartesianVector[Scalar] = new CartesianVector ( x*s, y*s )
  def /(s: Scalar): CartesianVector[Scalar] = new CartesianVector ( x/s, y/s )

  override def equals(obj: Any): Boolean = obj match {
    case that: AnyRef if this eq that => true
    case that: Vector[_] => x == that.x && y == that.y
    case _ => false
  }

  override def hashCode() = List(x, y).hashCode()

  override def toString: String = "CartesianVector(%f, %f)" format (x, y)

}

object CartesianVector {

  def apply[Scalar: IsScalar](x: Scalar, y: Scalar): CartesianVector[Scalar] = new CartesianVector(x, y)

}
