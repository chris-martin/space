package space.geometry
package dimension2

trait Triangle { self =>

  def arbitrarilyDirected: ThreePoints

  def directed(direction: RotationDirection): ThreePoints

  def circumcenter: Option[Vector]

  def circumcircle: Circle

  def perimeter: Triangle.Perimeter

  def interior: Triangle.Interior

  def exterior: Triangle.Exterior

  protected trait Self extends Triangle.Has {
    override def triangle: Triangle = self
  }
}

object Triangle {

  def apply(a: Vector, b: Vector, c: Vector): ThreePoints =
  ThreePoints(a, b, c)

  trait Has {

    def triangle: Triangle
  }

  trait Perimeter extends Has {

    def length: Double

    override def toString: String = s"Triangle.Perimeter($triangle)"
  }

  trait Interior extends Has {

    def area: Double

    override def toString: String = s"Triangle.Interior($triangle)"
  }

  trait Exterior extends Has {

    override def toString: String = s"Triangle.Exterior($triangle)"
  }
}

sealed case class ThreePoints(a: Vector, b: Vector, c: Vector) extends
Triangle { self =>

  override def arbitrarilyDirected: ThreePoints = this

  override def directed(direction: RotationDirection) =
  if (direction == self.direction) this else reverse

  def direction: RotationDirection = ???

  def vertices: Seq[Vector] = Seq(a, b, c)

  def edges: Seq[RaySegment] = Seq(a→b, b→c, c→a)

  def reverse: ThreePoints = ThreePoints(c, b, a)

  def shift: ThreePoints = ThreePoints(b, c, a)

  override def circumcenter: Option[Vector] = (a→b).bisector ∩ (b→c).bisector

  override def circumcircle: Circle = circumcenter match {
    case Some(x) => Circle(center = x, radius = (a→x).length)
    case None => edges.maxBy(_.length).circumcircle
  }

  object perimeter extends Triangle.Perimeter with Self {

    override def length: Double = edges.map(_.length).sum

    def traverse(d: Double): Vector = d % length match {
      case e if e < (a→b).length => (a→b).toRay.segment(e).destination
      case e => shift.perimeter.traverse(e - (a→b).length)
    }
  }

  object interior extends Triangle.Interior with Self {

    override def area: Double = (a→b).length * ((a→b).toLine → c).length / 2
  }

  object exterior extends Triangle.Exterior with Self
}
