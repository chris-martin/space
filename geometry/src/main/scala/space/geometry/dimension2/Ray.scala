package space.geometry
package dimension2

/** A half-line.
  *
  * {{{    ●--▸    }}}
  */
trait Ray {

  def source: Vector

  def angle: CircleRadians

  def rotate(a: ArbitraryRadians): Ray = Ray(source, angle + a)

  /** The ray segment of length 1 that has the same source and angle as
    * this ray.
    */
  def unitSegment: RaySegment = PointDifference(source, PolarVector(1, angle))

  def toDoubleRay: DoubleRay = DoubleRay(source, angle)

  /** The unique line by which this half-line is contained.
    */
  def toLine: Line = toDoubleRay.toLine

  def toPointAndCircleAngle: PointAndCircleAngle =
    PointAndCircleAngle(source, angle)

}

object Ray {

  def apply(source: Vector, angle: CircleRadians): PointAndCircleAngle =
    PointAndCircleAngle(source, angle)

}

sealed case class PointAndCircleAngle(source: Vector, angle: CircleRadians)
  extends Ray {

  override def toPointAndCircleAngle: PointAndCircleAngle = this

}
