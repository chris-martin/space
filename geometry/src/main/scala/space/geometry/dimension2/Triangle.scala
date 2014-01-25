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
Triangle {

  override def arbitrarilyDirected: ThreePoints = this

  override def directed(direction: RotationDirection) =
  if (direction == this.direction) this else reverse

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

  override val perimeter = ThreePoints.Perimeter(this)

  override val interior = ThreePoints.Interior(this)

  override val exterior = ThreePoints.Exterior(this)
}

object ThreePoints {

  sealed case class Perimeter(triangle: ThreePoints)
  extends Triangle.Perimeter {

    import triangle._

    override def length: Double = edges.map(_.length).sum

    def traverse(d: Double): Vector = d % length match {
      case e if e < (a→b).length => (a→b).toRay.segment(e).destination
      case e => shift.perimeter.traverse(e - (a→b).length)
    }
  }

  sealed case class Interior(triangle: ThreePoints) extends Triangle.Interior {

    import triangle._

    override def area: Double = (a→b).length * ((a→b).toLine → c).length / 2
  }

  sealed case class Exterior(triangle: ThreePoints) extends Triangle.Exterior
}
