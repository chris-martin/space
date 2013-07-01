package space.geometry
package dimension2

/** A point in the Euclidean plane.
  */
trait Vec[Scalar] {

  def x: Scalar
  def y: Scalar

  def xy: XY[Scalar]

  def magnitude: Scalar
  def unary_- : Vec[Scalar]

  def +(that: Vec[Scalar]): Vec[Scalar]
  def -(that: Vec[Scalar]): Vec[Scalar]

  def *(s: Scalar): Vec[Scalar]
  def /(s: Scalar): Vec[Scalar]

}
