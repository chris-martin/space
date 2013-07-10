package space.geometry.dimension2.angle

trait ArbitraryRadiansCompanion extends AngleCompanion[ArbitraryRadians] {

  override def apply(a: Double): ArbitraryRadians = new ArbitraryRadians(a)

}
