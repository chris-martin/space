package space.geometry
package dimension2

/** A half-line.
  *
  * {{{    ●--▸    }}}
  */
trait Ray {

  def source: Vector

  def angle: CircleRadians

  def rotate(a: AnyRadians): Ray = Ray(source, angle + a)

  /** The ray segment of length `length` that has the same source and angle
    * as this ray.
    */
  def segment(length: Double): RaySegment =
  PointDifference(source, PolarVector(length, angle))

  /** A ray segment that has the same source as this ray.
    */
  def arbitrarySegment: RaySegment = segment(1)

  def toDoubleRay: DoubleRay = DoubleRay(source, angle)

  /** The unique line by which this half-line is contained.
    */
  def toLine: Line = toDoubleRay.toLine

  def toPointAndCircleAngle: PointAndCircleAngle =
  PointAndCircleAngle(source, angle)

  /** If you walk the line on which this ray lies, in the direction
    * in which the ray points, do you see `x` on your left or your right?
    */
  def side(x: Vector): Side
}

object Ray {

  def apply(source: Vector, angle: CircleRadians): PointAndCircleAngle =
  PointAndCircleAngle(source, angle)
}

sealed case class PointAndCircleAngle(source: Vector, angle: CircleRadians)
extends Ray {

  override def toPointAndCircleAngle: PointAndCircleAngle = this

  override def side(x: Vector): Side =
  if (((x - source) cross arbitrarySegment.difference) > 0) Left else Right
}
