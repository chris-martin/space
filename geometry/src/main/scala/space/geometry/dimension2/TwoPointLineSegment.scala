package space.geometry
package dimension2

sealed case class TwoPointLineSegment(a: Vector, b: Vector) extends LineSegment {

  override def toLine: TwoPointLine = TwoPointLine(a, b)

  override def points: (Vector, Vector) = (a, b)

  override def length: Double = (b-a).magnitude

}
