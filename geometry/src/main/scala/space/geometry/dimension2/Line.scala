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

/** A line segment.
  *
  * {{{    ●--●    }}}
  */
trait LineSegment {

  def angle: SemicircleRadians

  /** The unique line by which this line segment is contained.
    */
  def toLine: Line

  def midpoint: Vector = arbitrarilyDirected.midpoint

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

  /** The ray segment of length 1 that has the same source and angle as this ray.
    */
  def unitSegment: RaySegment = PointDifference(source, PolarVector(1, angle))

  def toDoubleRay: DoubleRay = DoubleRay(source, angle)

  /** The unique line by which this half-line is contained.
    */
  def toLine: Line = toDoubleRay.toLine

}

object Ray {

  def apply(pivot: Vector, angle: CircleRadians): PointAndCircleAngle =
    PointAndCircleAngle(pivot, angle)

}

sealed case class PointAndCircleAngle(source: Vector, angle: CircleRadians)
  extends Ray

/** A line, and a point (the "pivot") on that line.
  *
  * {{{    ◂--●--▸    }}}
  */
trait DoubleRay { self =>

  def pivot: Vector

  def angle: SemicircleRadians

  def directed(angleSign: Sign): Ray =
    Ray(pivot, angle.toCircleRadians(angleSign))

  def arbitrarilyDirected: Ray = directed(Positive)

  /** Rotation about the pivot. The resulting `DoubleRay` has the same pivot.
    */
  def rotate(a: ArbitraryRadians): DoubleRay

  def toLine: Line = new Line {
    override def angle: SemicircleRadians = self.angle
    override def arbitraryRaySegment: RaySegment = self.arbitrarilyDirected.unitSegment
  }

}

object DoubleRay {

  def apply(point: Vector, angle: SemicircleRadians): PointAndSemicircleAngle =
    PointAndSemicircleAngle(point, angle)

}

sealed case class PointAndSemicircleAngle(pivot: Vector, angle: SemicircleRadians)
  extends DoubleRay {

  override def rotate(a: ArbitraryRadians): PointAndSemicircleAngle =
    DoubleRay(pivot, angle + a)

}

/** {{{    ●--▸●   }}}
  */
trait RaySegment { self =>

  def source: Vector
  def destination: Vector

  def reverse: RaySegment
  def angle: CircleRadians = difference.angle
  def length: Double = difference.magnitude
  def difference: Vector
  def midpoint: Vector = source + difference/2

  def toRay: Ray = Ray(source, angle)
  def toDoubleRay: DoubleRay = DoubleRay(source, angle)

  def toLineSegment: LineSegment = new LineSegment {
    override def angle = self.angle
    override def directed(angleSign: Sign): RaySegment = self.withAngleSign(angleSign)
    override def toLine: Line = self.toLine
  }

  def withAngleSign(angleSign: Sign): RaySegment =
    if (angle.sign == angleSign) this else reverse

  def toLine: Line = new Line {
    override def angle: SemicircleRadians = self.angle
    override def arbitraryRaySegment: RaySegment = self
  }

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

  override def difference: Vector = destination - source

  override def reverse: TwoPoints =
    TwoPoints(source = destination, destination = source)

}

sealed case class PointDifference(source: Vector, difference: Vector)
  extends RaySegment {

  override def destination: Vector = source + difference

  override def reverse: PointDifference =
    PointDifference(destination, -difference)

}
