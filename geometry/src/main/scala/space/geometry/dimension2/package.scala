package space.geometry

package object dimension2 extends Approximations {

  implicit class DoubleEnrichedForDimension2(x: Double) {
    def radians: ArbitraryRadians = ArbitraryRadians(x)
  }

}
