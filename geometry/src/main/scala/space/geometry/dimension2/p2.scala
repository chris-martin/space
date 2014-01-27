package space.geometry
package dimension2

/**
 * A supertrait for a line subset with two end points. The endpoints may be
 * unordered (a "line segment") or ordered (a "ray segment").
 */
trait LineSegmentLike {

  def length: Double

  def midpoint: Point

  def angle: Radians

  /** A double ray orthogonal to this segment, with a pivot at this segment's
    * midpoint.
    */
  def bisector: DoubleRay =
    DoubleRay(pivot = midpoint, angle = (angle.toDouble + Pi/2).radians)

  /** The minimum-radius circle containing both endpoints.
    */
  def circumcircle: Circle = Circle(center = midpoint, radius = length/2)

  /** The unique line by which this segment is contained.
    */
  def toLine: Line

  /** Rotation about the midpoint.
    */
  def rotate(angle: AnyRadians): LineSegmentLike = rotate(angle, midpoint)

  /** Rotation about a pivot.
    */
  def rotate(angle: AnyRadians, pivot: Point): LineSegmentLike
}

/** A line segment.
  *
  * {{{    ●--●    }}}
  */
trait LineSegment extends LineSegmentLike {

  override def angle: SemicircleRadians

  override def midpoint: Point = arbitrarilyDirected.midpoint

  def directed(angleSign: Sign): RaySegment

  def arbitrarilyDirected: RaySegment = directed(Positive)

  override def rotate(angle: AnyRadians): LineSegment =
    rotate(angle, midpoint)

  override def rotate(angle: AnyRadians, pivot: Point): LineSegment

  /** A double ray lying on the same line as this segment, with its pivot
    * at this segment's midpoint.
    */
  def toDoubleRay: DoubleRay = PointAndSemicircleAngle(midpoint, angle)

  def withLength(newLength: Double): LineSegment =
    toDoubleRay.toLineSegment(newLength)
}

object LineSegment {

  def apply(a: Point, b: Point): LineSegment =
    TwoPoints(a, b).toLineSegment
}

/** {{{    ●--▸●    }}}
  */
trait RaySegment extends LineSegmentLike { self =>

  def source: Point

  def destination: Point

  def reverse: RaySegment

  override def angle: CircleRadians = difference.angle

  override def length: Double = difference.magnitude

  def difference: Point

  override def midpoint: Point = source + difference/2

  def toRay: Ray = Ray(source, angle)

  def toDoubleRay: DoubleRay = DoubleRay(source, angle)

  override def rotate(angle: AnyRadians): RaySegment =
    rotate(angle, midpoint)

  override def rotate(angle: AnyRadians, pivot: Point): RaySegment

  def toLineSegment: LineSegment = new LineSegment {

    override def directed(angleSign: Sign): RaySegment =
      self.withAngleSign(angleSign)

    override def length: Double = self.length

    override def angle: SemicircleRadians = self.angle

    override def toLine: Line = self.toLine

    override def rotate(angle: AnyRadians, pivot: Point): LineSegment =
      self.rotate(angle, pivot).toLineSegment

    override def toString: String = s"LineSegment($self)"
  }

  def withAngleSign(angleSign: Sign): RaySegment =
    if (angle.sign == angleSign) this else reverse

  override def toLine: Line = new Line {

    override def arbitraryDoubleRay: DoubleRay =
      DoubleRay(pivot = self.source, angle = self.angle)

    override def toString: String = s"Line($self)"
  }

  def toTwoPoints: TwoPoints = TwoPoints(source, destination)

  def toPointDifference: PointDifference = PointDifference(source, difference)

  def toSeq: Seq[Point] = Seq(source, destination)

  def toTuple: (Point, Point) = (source, destination)
}

object RaySegment {

  def apply(a: Point, b: Point): RaySegment = TwoPoints(a, b)

  def lineIntersection(ab: RaySegment, cd: RaySegment): Option[Point] = {

    val a = ab.source
    val b = ab.destination
    val c = cd.source
    val d = cd.destination

    (a.x-b.x)*(c.y-d.y) - (a.y-b.y)*(c.x-d.x) match {

      case 0 => None

      case denominator => Some {
        val r = a.x * b.y - a.y * b.x
        val s = c.x * d.y - c.y * d.x
        val x = r*(c.x - d.x) - s*(a.x - b.x)
        val y = r*(c.y - d.y) - s*(a.y - b.y)
        xy(x, y) / denominator
      }
    }
  }
}

sealed case class TwoPoints(a: Point, b: Point) extends RaySegment {

  override def source = a

  override def destination = b

  override def toTwoPoints: TwoPoints = this

  override def difference: Point = destination - source

  override def reverse: TwoPoints = TwoPoints(b, a)

  override def rotate(angle: AnyRadians, pivot: Point): TwoPoints =
    TwoPoints(source.rotate(angle, pivot), destination.rotate(angle, pivot))
}

sealed case class PointDifference(source: Point, difference: Point)
    extends RaySegment {

  override def toPointDifference: PointDifference = this

  override def destination: Point = source + difference

  override def reverse: PointDifference =
    PointDifference(destination, -difference)

  override def rotate(angle: AnyRadians, pivot: Point): PointDifference =
    PointDifference(source.rotate(angle, pivot), difference.rotate(angle))
}

/** A half-line.
  *
  * {{{    ●--▸    }}}
  */
trait Ray {

  def source: Point

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
  def side(x: Point): Side
}

object Ray {

  def apply(source: Point, angle: CircleRadians): PointAndCircleAngle =
    PointAndCircleAngle(source, angle)
}

sealed case class PointAndCircleAngle(source: Point, angle: CircleRadians)
    extends Ray {

  override def toPointAndCircleAngle: PointAndCircleAngle = this

  override def side(x: Point): Side =
    if (((x - source) cross arbitrarySegment.difference) > 0) Left else Right
}

/** A supertrait for line types.
  */
trait LineLike {

  def angle: SemicircleRadians

  def arbitrarySegment: LineSegment

  def toLine: Line

  def arbitraryRaySegment: RaySegment

  def ∩(that: LineLike): Option[Point] =
    RaySegment.lineIntersection(
      this.arbitraryRaySegment,
      that.arbitraryRaySegment
    )

  /** The point `b` on this line that minimizes the distance
    * between `a` and `b`.
    */
  def pointClosestTo(a: Point): Point =
    ( this ∩ PointAndSemicircleAngle(a, angle.orthogonal) ).get

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

  def apply(a: Point, b: Point): Line = LineSegment(a, b).toLine
}

/** A line, and a point (the "pivot") on that line.
  *
  * {{{    ◂--●--▸    }}}
  */
trait DoubleRay extends LineLike { self =>

  def pivot: Point

  def directed(angleSign: Sign): Ray =
    Ray(pivot, angle.toCircleRadians(angleSign))

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

  def apply(pivot: Point, angle: SemicircleRadians): PointAndSemicircleAngle =
    PointAndSemicircleAngle(pivot, angle)
}

sealed case class PointAndSemicircleAngle(pivot: Point,
    angle: SemicircleRadians) extends DoubleRay {

  override def toPointAndSemicircleAngle: PointAndSemicircleAngle = this

  override def rotate(a: AnyRadians): PointAndSemicircleAngle =
    PointAndSemicircleAngle(pivot, angle + a)

  override def toLineSegment(length: Double): LineSegment = {
    val offset = PolarVector(length/2, angle.toCircleRadiansArbitrarily)
    TwoPoints(pivot - offset, pivot + offset).toLineSegment
  }
}
