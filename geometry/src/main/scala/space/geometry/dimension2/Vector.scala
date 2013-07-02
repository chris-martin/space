package space.geometry
package dimension2

/** A point in the Euclidean plane.
  */
trait Vector[S] {

  implicit def scalar: Scalar[S]

  def x: S
  def y: S

  def toCartesian: CartesianVector[S] = CartesianVector[S](x, y)

  def magnitude: S
  def unary_- : Vector[S]

  def +(that: Vector[S]): Vector[S]
  def -(that: Vector[S]): Vector[S]

  def *(s: S): Vector[S]
  def /(s: S): Vector[S]

}
