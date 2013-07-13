package space.geometry
package dimension2

trait LineSegment {

  def points: (Vector, Vector)

  def toLine: Line

  def length: Double

}
