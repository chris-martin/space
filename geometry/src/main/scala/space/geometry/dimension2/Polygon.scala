package space.geometry
package dimension2

trait Polygon {

  def arbitraryPath: Cycle

  def area: Double

  def perimeter: Polygon.Perimeter
  def interior: Polygon.Interior
  def exterior: Polygon.Exterior
}

trait DefinedByPolygon[+A <: Polygon] {

  def polygon: A
}

object Polygon {

  trait PerimeterOf[+A <: Polygon] extends DefinedByPolygon[A] {

    def length: Double
  }

  trait InteriorOf[+A <: Polygon] extends DefinedByPolygon[A] {

    def area: Double = polygon.area
  }

  trait ExteriorOf[+A <: Polygon] extends DefinedByPolygon[A]

  type Perimeter  = PerimeterOf[Polygon]
  type Interior = InteriorOf[Polygon]
  type Exterior = ExteriorOf[Polygon]
}
