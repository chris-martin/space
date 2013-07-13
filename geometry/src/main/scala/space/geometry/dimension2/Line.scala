package space.geometry
package dimension2

/**     ◂--▸
  */
trait Line {
  def angle: SemicircleRadians
  def intersects(that: Line): Boolean = angle != that.angle
}

/**     ●--●
  */
trait LineSegment {
  def angle: SemicircleRadians
  def toLine: Line
  def toRaySegment(angleSign: Sign): RaySegment
}

/**     ●--▸
  */
sealed case class Ray(point: Vector, angle: CircleRadians) {

  def rotate(a: ArbitraryRadians): Ray = Ray(point, angle + a)

  def toSegment(magnitude: Double = 1): RaySegment =
    PointDifference(point, PolarVector(magnitude, angle))

  def toDoubleRay: DoubleRay = DoubleRay(point, angle)

  def toLine: Line = toDoubleRay.toLine

}

/**     ◂--●--▸
  */
sealed case class DoubleRay(point: Vector, angle: SemicircleRadians) { self =>

  def directed(angleSign: Sign): Ray =
    Ray(point, angle.toCircleRadians(angleSign))

  def rotate(a: ArbitraryRadians): DoubleRay =
    DoubleRay(point, angle + a)

  def toLine: Line = new Line {
    override def angle: SemicircleRadians = self.angle
  }

}

/**     ●--▸●
  */
trait RaySegment { self =>

  def source: Vector
  def destination: Vector

  def reverse: RaySegment
  def angle: CircleRadians = difference.angle
  def length: Double = difference.magnitude
  def difference: Vector

  def toRay: Ray = Ray(source, angle)
  def toDoubleRay: DoubleRay = DoubleRay(source, angle)

  def toLineSegment: LineSegment = new LineSegment {

    override def angle = self.angle

    override def toRaySegment(angleSign: Sign): RaySegment =
      PointDifference(
        source = self.source,
        difference = PolarVector(
          magnitude = self.length,
          angle = self.angle.toCircleRadians(angleSign)
        )
      )

    override def toLine = new Line {

      override def angle: SemicircleRadians = self.angle

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
