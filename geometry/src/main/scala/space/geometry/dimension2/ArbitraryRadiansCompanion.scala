package space.geometry
package dimension2

trait ArbitraryRadiansCompanion extends AngleCompanion[ArbitraryRadians] {

  override def apply(a: Double): ArbitraryRadians = new ArbitraryRadians(a)

}
