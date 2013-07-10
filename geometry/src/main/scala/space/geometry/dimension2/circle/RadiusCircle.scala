package space.geometry.dimension2.circle

sealed case class RadiusCircle(center: Vector, radius: Double) extends Circle {

  override def toRadiusCircle = this

}
