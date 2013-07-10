package space.geometry
package dimension2
package vector

import angle._

/** A point in the Euclidean plane.
  */
trait Vector {

  def x: Double
  def y: Double

  def toCartesian: CartesianVector
  def toPolar: PolarVector

  def magnitude: Double
  def unary_- : Vector

  def +(that: Vector): Vector
  def -(that: Vector): Vector

  def *(s: Double): Vector
  def /(s: Double): Vector

  def rotate(a: CircleRadians): Vector

}
