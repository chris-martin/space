package space.geometry.dimension2

trait Triangle {

  def arbitrarilyDirected: ThreePoints

  def directed(direction: RotationDirection): ThreePoints

  def circumcircle: Circle

}

sealed case class ThreePoints(a: Vector, b: Vector, c: Vector) extends
Triangle { self =>

  def arbitrarilyDirected: ThreePoints = this

  def directed(direction: RotationDirection) =
  if (direction == self.direction) this else reverse

  def direction: RotationDirection = ???

  def reverse: ThreePoints = ThreePoints(c, b, a)

  def circumcircle: Circle = ???

}
