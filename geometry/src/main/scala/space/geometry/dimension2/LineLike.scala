package space.geometry
package dimension2

/** A supertrait for line types.
  */
trait LineLike {

  def angle: SemicircleRadians

  def arbitrarySegment: LineSegment

  def toLine: Line

  def arbitraryRaySegment: RaySegment

  def ∩(that: LineLike): Option[Vector] = RaySegment.lineIntersection(
  this.arbitraryRaySegment, that.arbitraryRaySegment)

  /** The point `b` on this line that minimizes the distance
    * between `a` and `b`.
    */
  def pointClosestTo(a: Vector): Vector =
  (this ∩ PointAndSemicircleAngle(a, angle.orthogonal)).get

}

/** A line.
  *
  * {{{    ◂--▸    }}}
  */
trait Line extends LineLike {

  def arbitraryDoubleRay: DoubleRay

  override def arbitrarySegment: LineSegment =
  arbitraryDoubleRay.arbitrarySegment

  override def arbitraryRaySegment: RaySegment =
  arbitrarySegment.arbitrarilyDirected

  override def angle: SemicircleRadians = arbitraryDoubleRay.angle

  override def toLine: Line = this
}

object Line {

  def apply(a: Vector, b: Vector): Line = LineSegment(a, b).toLine
}

/** A line, and a point (the "pivot") on that line.
  *
  * {{{    ◂--●--▸    }}}
  */
trait DoubleRay extends LineLike { self =>

  def pivot: Vector

  def directed(angleSign: Sign): Ray = Ray(pivot,
  angle.toCircleRadians(angleSign))

  def arbitrarilyDirected: Ray = directed(Positive)

  override def arbitraryRaySegment: RaySegment =
  arbitrarilyDirected.segment(1)

  override def arbitrarySegment: LineSegment =
  arbitraryRaySegment.toLineSegment

  /** Rotation about the pivot. The resulting `DoubleRay` has the same pivot.
    */
  def rotate(a: AnyRadians): DoubleRay

  override def toLine: Line = new Line {

    override def arbitraryDoubleRay = self
  }

  def toPointAndSemicircleAngle: PointAndSemicircleAngle =
  PointAndSemicircleAngle(pivot = pivot, angle = angle)

  /** A line segment lying on the same line as this ray segment, with
    * its midpoint at the ray segments pivot, having length `length`.
    */
  def toLineSegment(length: Double): LineSegment
}

object DoubleRay {

  def apply(pivot: Vector, angle: SemicircleRadians): PointAndSemicircleAngle =
  PointAndSemicircleAngle(pivot, angle)
}

sealed case class PointAndSemicircleAngle(pivot: Vector, angle:
SemicircleRadians) extends DoubleRay {

  override def toPointAndSemicircleAngle: PointAndSemicircleAngle = this

  override def rotate(a: AnyRadians): PointAndSemicircleAngle =
  PointAndSemicircleAngle(pivot, angle + a)

  override def toLineSegment(length: Double): LineSegment = {

    val offset = PolarVector(length/2, angle.toCircleRadiansArbitrarily)

    TwoPoints(pivot - offset, pivot + offset).toLineSegment
  }
}
