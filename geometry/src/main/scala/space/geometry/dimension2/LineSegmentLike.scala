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

sealed case class TwoPoints(source: Point, destination: Point)
    extends RaySegment {

  override def toTwoPoints: TwoPoints = this

  override def difference: Point = destination - source

  override def reverse: TwoPoints =
    TwoPoints(source = destination, destination = source)

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
