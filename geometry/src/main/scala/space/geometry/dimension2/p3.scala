package space.geometry
package dimension2

trait Triangle extends Polygon {

  override def arbitraryPath: ThreePoints

  def circumcenter: Option[Point]

  def circumcircle: Circle

  override def perimeter: Triangle.Perimeter
  override def interior: Triangle.Interior
  override def exterior: Triangle.Exterior
}

trait DefinedByTriangle[+A <: Triangle] extends DefinedByPolygon[A] {

  def triangle: A = polygon

  def polygon: A
}

object Triangle {

  def apply(a: Point, b: Point, c: Point): ThreePoints = ThreePoints(a, b, c)

  trait PerimeterOf[+A <: Triangle] extends Polygon.PerimeterOf[A]
      with DefinedByTriangle[A] {

    override def toString: String = s"Triangle.Perimeter($triangle)"
  }

  trait InteriorOf[+A <: Triangle] extends Polygon.InteriorOf[A]
      with DefinedByTriangle[A] {

    override def toString: String = s"Triangle.Interior($triangle)"
  }

  trait ExteriorOf[+A <: Triangle] extends Polygon.ExteriorOf[A]
      with DefinedByTriangle[A] {

    override def toString: String = s"Triangle.Exterior($triangle)"
  }

  type Perimeter = PerimeterOf[Triangle]
  type Interior = InteriorOf[Triangle]
  type Exterior = ExteriorOf[Triangle]
}

sealed case class ThreePoints(a: Point, b: Point, c: Point)
    extends Cycle with Triangle {

  override def arbitraryPath: ThreePoints = this

  override def area: Double = (a→b).length * ((a→b).toLine → c).length / 2

  def withDirection(direction: RotationDirection): ThreePoints =
    if (direction == this.direction) this else reverse

  def direction: RotationDirection = ???

  override def vertices: Seq[Point] = Seq(a, b, c)

  def reverse: ThreePoints = ThreePoints(c, b, a)

  def shift: ThreePoints = ThreePoints(b, c, a)

  override def circumcenter: Option[Point] = (a→b).bisector ∩ (b→c).bisector

  override def circumcircle: Circle =
    circumcenter match {
      case Some(x) => Circle(center = x, radius = (a→x).length)
      case None => edges.maxBy(_.length).circumcircle
    }

  override val perimeter = ThreePoints.Perimeter(this)
  override val interior = ThreePoints.Interior(this)
  override val exterior = ThreePoints.Exterior(this)
}

object ThreePoints {

  sealed case class Perimeter(polygon: ThreePoints)
      extends Triangle.PerimeterOf[ThreePoints] {

    override def length: Double = triangle.edges.map(_.length).sum

    def traverse(d: Double): Point = {
      val ab = triangle.a → triangle.b
      d % length match {
        case e if e < ab.length => ab.toRay.segment(e).destination
        case e => triangle.shift.perimeter.traverse(e - ab.length)
      }
    }
  }

  sealed case class Interior(polygon: ThreePoints)
      extends Triangle.InteriorOf[ThreePoints]

  sealed case class Exterior(polygon: ThreePoints)
      extends Triangle.ExteriorOf[ThreePoints]
}
