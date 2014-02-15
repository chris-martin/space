package space.geometry
package dimension2

trait Cycle {

  def vertices: Seq[Point]

  def edges: Seq[RaySegment] = {
    val vs = vertices
    ((vs.tail :+ vs.head) zip vs) map { case (a: Point, b: Point) => a → b }
  }

  def length: Double = edges.map(_.length).sum
}

object Cycle {

  def apply(a: Point, b: Point, c: Point): ThreePoints = ThreePoints(a, b, c)

  def apply(a: Point, b: Point, c: Point, d: Point): FourPoints =
    FourPoints(a, b, c, d)
}
