package space.geometry
package dimension2

import space.approximation.Approximation

trait Approximations {

  trait AngleApproximation[A <: Radians] extends Approximation[A] {

    override def apply(a: A, b: A)(implicit tolerance: Tolerance): Boolean =
      $(a, b)(_.toDouble)
  }

  implicit object ArbitraryRadiansApproximation
      extends AngleApproximation[AnyRadians]

  implicit object CircleRadiansApproximation
      extends AngleApproximation[CircleRadians]

  implicit object SemicircleRadiansApproximation
      extends AngleApproximation[SemicircleRadians]

  implicit object CartesianVectorApproximation
      extends Approximation[CartesianVector] {

    override def apply(a: CartesianVector, b: CartesianVector)
        (implicit tolerance: Tolerance): Boolean =
      $(a, b)(_.x) && $(a, b)(_.y)
  }

  implicit object PolarVectorApproximation extends Approximation[PolarVector] {

    override def apply(a: PolarVector, b: PolarVector)
        (implicit tolerance: Tolerance): Boolean =
      $(a, b)(_.magnitude) && $(a, b)(_.angle)
  }

  implicit object PointApproximation extends Approximation[Point] {

    override def apply(a: Point, b: Point)
        (implicit tolerance: Tolerance): Boolean = $(a, b)(_.toCartesian)
  }

  implicit object PointAndCircleAngleApproximation
      extends Approximation[PointAndCircleAngle] {

    override def apply(a: PointAndCircleAngle, b: PointAndCircleAngle)
        (implicit tolerance: Tolerance): Boolean =
      $(a, b)(_.source) && $(a, b)(_.angle)
  }

  implicit object LineSegmentApproximation extends Approximation[LineSegment] {

    override def apply(a: LineSegment, b: LineSegment)
        (implicit tolerance: Tolerance): Boolean =
      $(a, b)(_.midpoint) && $(a, b)(_.length) && $(a, b)(_.angle)
  }

  implicit object RayApproximation extends Approximation[Ray] {

    override def apply(a: Ray, b: Ray)
        (implicit tolerance: Tolerance): Boolean =
      $(a, b)(_.toPointAndCircleAngle)
  }

  implicit object PointAndSemicircleAngleApproximation
      extends Approximation[PointAndSemicircleAngle] {

    override def apply(a: PointAndSemicircleAngle, b: PointAndSemicircleAngle)
        (implicit tolerance: Tolerance): Boolean =
      $(a, b)(_.pivot) && $(a, b)(_.angle)
  }

  implicit object DoubleRayApproximation extends Approximation[DoubleRay] {

    override def apply(a: DoubleRay, b: DoubleRay)
        (implicit tolerance: Tolerance): Boolean =
      $(a, b)(_.toPointAndSemicircleAngle)
  }

  implicit object TwoPointsApproximation extends Approximation[TwoPoints] {

    override def apply(a: TwoPoints, b: TwoPoints)
        (implicit tolerance: Tolerance): Boolean =
      $(a, b)(_.source) && $(a, b)(_.destination)
  }

  implicit object PointDifferenceApproximation
      extends Approximation[PointDifference] {

    override def apply(a: PointDifference, b: PointDifference)
        (implicit tolerance: Tolerance): Boolean =
      $(a, b)(_.source) && $(a, b)(_.difference)
  }

  implicit object CircleApproximation extends Approximation[Circle] {

    override def apply(a: Circle, b: Circle)
        (implicit tolerance: Tolerance): Boolean =
      $(a, b)(_.center) && $(a, b)(_.radius)
  }

  implicit object DiagonalAndCornerRectangleApproximation
      extends Approximation[DiagonalAndCornerRectangle] {

    override def apply
        (a: DiagonalAndCornerRectangle, b: DiagonalAndCornerRectangle)
        (implicit tolerance: Tolerance): Boolean =
      $(a, b)(_.diagonal) && $(a, b)(_.corner)
  }
}
