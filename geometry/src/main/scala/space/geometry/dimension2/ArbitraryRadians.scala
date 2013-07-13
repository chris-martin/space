package space.geometry
package dimension2

class ArbitraryRadians (val toDouble: Double)
    extends AnyVal with Angle[ArbitraryRadians] {

  override protected def companion: AngleCompanion[ArbitraryRadians] = ArbitraryRadians

  override def toString = "ArbitraryRadians(%f)" format toDouble

}

object ArbitraryRadians extends ArbitraryRadiansCompanion
