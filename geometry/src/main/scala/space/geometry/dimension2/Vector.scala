package space.geometry
package dimension2

/** A point in the Euclidean plane.
  */
trait Vector[Scalar] {

  def x: Scalar
  def y: Scalar

  def xy: CartesianVector[Scalar]

  def magnitude: Scalar
  def unary_- : Vector[Scalar]

  def +(that: Vector[Scalar]): Vector[Scalar]
  def -(that: Vector[Scalar]): Vector[Scalar]

  def *(s: Scalar): Vector[Scalar]
  def /(s: Scalar): Vector[Scalar]

}
