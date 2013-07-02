package space.geometry
package dimension2

/** A two-dimensional vector defined by X and Y coordinates.
  */
sealed case class CartesianVector[S](x: S, y: S)
    (implicit val scalar: Scalar[S]) extends Vector[S] {

  import scalar._

  override def toCartesian: CartesianVector[S] = this

  override def magnitude: S = (x.square + y.square).squareRoot

  override def unary_- : CartesianVector[S] = CartesianVector ( -x, -y )

  override def +(that: Vector[S]) = CartesianVector ( x + that.x, y + that.y )
  override def -(that: Vector[S]) = CartesianVector ( x - that.x, y - that.y )

  override def *(s: S): CartesianVector[S] = new CartesianVector ( x*s, y*s )
  override def /(s: S): CartesianVector[S] = new CartesianVector ( x/s, y/s )

}
