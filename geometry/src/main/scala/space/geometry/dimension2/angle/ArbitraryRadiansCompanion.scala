package space.geometry
package dimension2
package angle

trait ArbitraryRadiansCompanion extends AngleCompanion[ArbitraryRadians] {

  override def apply(a: Double): ArbitraryRadians = new ArbitraryRadians(a)

}
