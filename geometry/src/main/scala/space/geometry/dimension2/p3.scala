package space.geometry
package dimension2

trait Triangle extends Polygon {

  override def arbitraryPath: ThreePoints

  def circumcenter: Option[Point]

  def circumcircle: Circle
}

object Triangle {

  def apply(a: Point, b: Point, c: Point): ThreePoints = ThreePoints(a, b, c)
}

sealed case class ThreePoints(a: Point, b: Point, c: Point)
    extends Cycle with Triangle {

  override def arbitraryPath: ThreePoints = this

  override def area: Double = distance(a, b) * distance(Line(a, b), c) / 2

  def withDirection(direction: RotationDirection): ThreePoints =
    if (direction == this.direction) this else reverse

  def direction: RotationDirection = ???

  override def vertices: Seq[Point] = Seq(a, b, c)

  def reverse: ThreePoints = ThreePoints(c, b, a)

  def shift: ThreePoints = ThreePoints(b, c, a)

  override def circumcenter: Option[Point] =
    (a → b).bisector ∩ (b → c).bisector

  override def circumcircle: Circle =
    circumcenter match {
      case Some(x) => Circle(center = x, radius = distance(a, x))
      case None => edges.maxBy(_.length).circumcircle
    }

  def traverse(d: Double): Point = d % length match {
    case e if e < distance(a, b) => Ray(a → b).segmentWithLength(e).destination
    case e => shift.traverse(e - distance(a, b))
  }
}
