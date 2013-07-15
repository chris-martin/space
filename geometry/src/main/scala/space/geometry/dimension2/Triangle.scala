package space.geometry
package dimension2

trait Triangle {

  def arbitrarilyDirected: ThreePoints

  def directed(direction: RotationDirection): ThreePoints

  def circumcenter: Option[Vector]

  def circumcircle: Circle

}

sealed case class ThreePoints(a: Vector, b: Vector, c: Vector) extends
Triangle { self =>

  override def arbitrarilyDirected: ThreePoints = this

  override def directed(direction: RotationDirection) =
  if (direction == self.direction) this else reverse

  def direction: RotationDirection = ???

  def reverse: ThreePoints = ThreePoints(c, b, a)

  override def circumcenter: Option[Vector] = (a→b).bisector ∩ (b→c).bisector

  override def circumcircle: Circle = ???

}
