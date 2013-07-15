package space.geometry
package dimension2

/** A line.
  *
  * {{{    ◂--▸    }}}
  */
trait Line {

  def angle: SemicircleRadians

  def arbitraryRaySegment: RaySegment

  def ∩(that: Line): Option[Vector] =
  RaySegment.lineIntersection(
    this.arbitraryRaySegment,
    that.arbitraryRaySegment
  )

}

/**
 * A supertrait for a line subset with two end points. The endpoints may be
 * unordered (a "line segment") or ordered (a "ray segment").
 */
trait LineSegmentLike {

  def length: Double

  def midpoint: Vector

  def angle: Angle[_]

  /** A double ray orthogonal to this segment, with a pivot at this segment's
    * midpoint.
    */
  def bisector: DoubleRay = DoubleRay(midpoint, Angle(angle.toDouble + Pi/2))

  /** The unique line by which this segment is contained.
    */
  def toLine: Line

}

/** A line segment.
  *
  * {{{    ●--●    }}}
  */
trait LineSegment extends LineSegmentLike {

  override def angle: SemicircleRadians

  override def midpoint: Vector = arbitrarilyDirected.midpoint

  def directed(angleSign: Sign): RaySegment

  def arbitrarilyDirected: RaySegment = directed(Positive)

}

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

/** A line, and a point (the "pivot") on that line.
  *
  * {{{    ◂--●--▸    }}}
  */
trait DoubleRay { self =>

  def pivot: Vector

  def angle: SemicircleRadians

  def directed(angleSign: Sign): Ray = Ray(pivot,
  angle.toCircleRadians(angleSign))

  def arbitrarilyDirected: Ray = directed(Positive)

  /** Rotation about the pivot. The resulting `DoubleRay` has the same pivot.
    */
  def rotate(a: ArbitraryRadians): DoubleRay

  def toLine: Line = new Line {

    override def angle: SemicircleRadians = self.angle

    override def arbitraryRaySegment: RaySegment =
    self.arbitrarilyDirected.unitSegment

  }

  def toPointAndSemicircleAngle: PointAndSemicircleAngle =
  PointAndSemicircleAngle(pivot = pivot, angle = angle)

}

object DoubleRay {

  def apply(point: Vector, angle: SemicircleRadians): PointAndSemicircleAngle =
  PointAndSemicircleAngle(point, angle)

}

sealed case class PointAndSemicircleAngle(pivot: Vector, angle:
SemicircleRadians) extends DoubleRay {

  override def toPointAndSemicircleAngle: PointAndSemicircleAngle = this

  override def rotate(a: ArbitraryRadians): PointAndSemicircleAngle =
  DoubleRay(pivot, angle + a)

}

/** {{{    ●--▸●   }}}
  */
trait RaySegment extends LineSegmentLike { self =>

  def source: Vector

  def destination: Vector

  def reverse: RaySegment

  override def angle: CircleRadians = difference.angle

  override def length: Double = difference.magnitude

  def difference: Vector

  override def midpoint: Vector = source + difference/2

  def toRay: Ray = Ray(source, angle)

  def toDoubleRay: DoubleRay = DoubleRay(source, angle)

  def toLineSegment: LineSegment = new LineSegment {

    override def length: Double = self.length

    override def angle: SemicircleRadians = self.angle

    override def directed(angleSign: Sign): RaySegment =
    self.withAngleSign(angleSign)

    override def toLine: Line = self.toLine

  }

  def withAngleSign(angleSign: Sign): RaySegment =
  if (angle.sign == angleSign) this else reverse

  override def toLine: Line = new Line {

    override def angle: SemicircleRadians = self.angle

    override def arbitraryRaySegment: RaySegment = self

  }

  def toTwoPoints: TwoPoints = TwoPoints(source, destination)

  def toPointDifference: PointDifference = PointDifference(source, difference)

}

object RaySegment {

  def lineIntersection(ab: RaySegment, cd: RaySegment): Option[Vector] = {

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
        CartesianVector(x, y) / denominator
      }
    }
  }

}

sealed case class TwoPoints(source: Vector, destination: Vector)
extends RaySegment {

  override def toTwoPoints: TwoPoints = this

  override def difference: Vector = destination - source

  override def reverse: TwoPoints =
  TwoPoints(source = destination, destination = source)

}

sealed case class PointDifference(source: Vector, difference: Vector)
extends RaySegment {

  override def toPointDifference: PointDifference = this

  override def destination: Vector = source + difference

  override def reverse: PointDifference =
  PointDifference(destination, -difference)

}

trait LineApproximations {

  implicit object PointAndCircleAngleApproximation extends
  Approximation[PointAndCircleAngle] {

    override def apply(a: PointAndCircleAngle, b: PointAndCircleAngle)
    (implicit tolerance: Tolerance) = $(a, b)(_.source) && $(a, b)(_.angle)

  }

  implicit object RayApproximation extends Approximation[Ray] {

    override def apply(a: Ray, b: Ray)(implicit tolerance: Tolerance) =
    $(a, b)(_.toPointAndCircleAngle)

  }

  implicit object PointAndSemicircleAngleApproximation extends
  Approximation[PointAndSemicircleAngle] {

    override def apply(a: PointAndSemicircleAngle, b: PointAndSemicircleAngle)
    (implicit tolerance: Tolerance) = $(a, b)(_.pivot) && $(a, b)(_.angle)

  }

  implicit object DoubleRayApproximation extends Approximation[DoubleRay] {

    override def apply(a: DoubleRay, b: DoubleRay)(implicit tolerance:
    Tolerance) = $(a, b)(_.toPointAndSemicircleAngle)

  }

  implicit object TwoPointsApproximation extends Approximation[TwoPoints] {

    override def apply(a: TwoPoints, b: TwoPoints)(implicit tolerance:
    Tolerance) = $(a, b)(_.source) && $(a, b)(_.destination)

  }

}
