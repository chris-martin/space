package space.geometry
package dimension2

/** A two-dimensional vector defined by X and Y coordinates.
  */
sealed class CartesianVector[S](val x: S, val y: S)
    (implicit val scalar: Scalar[S]) extends Vector[S] {

  import scalar._

  override def toCartesian: CartesianVector[S] = this

  override def magnitude: S = (x.square + y.square).squareRoot

  override def unary_- : CartesianVector[S] = CartesianVector ( -x, -y )

  override def +(that: Vector[S]) = CartesianVector ( x + that.x, y + that.y )
  override def -(that: Vector[S]) = CartesianVector ( x - that.x, y - that.y )

  override def *(s: S): CartesianVector[S] = new CartesianVector ( x*s, y*s )
  override def /(s: S): CartesianVector[S] = new CartesianVector ( x/s, y/s )

  override def equals(obj: Any): Boolean = obj match {
    case that: AnyRef if this eq that => true
    case that: CartesianVector[_] => x == that.x && y == that.y
    case _ => false
  }

  override def hashCode() = List(x, y).hashCode()

  override def toString: String = "CartesianVector(%f, %f)" format (x, y)

}

object CartesianVector {

  def apply[S: Scalar](x: S, y: S): CartesianVector[S] = new CartesianVector(x, y)

}
