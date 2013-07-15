package space.geometry

package object dimension2 extends Approximations {

  implicit class DoubleEnrichedForDimension2(x: Double) {

    def radians: ArbitraryRadians = ArbitraryRadians(x)

  }

  type RotationDirection = Sign

  val Clockwise: RotationDirection = Negative

  val Counterclockwise: RotationDirection = Positive

  implicit final class ArrowAssoc(left: Vector) {
    def ->(right: Vector): TwoPoints = TwoPoints(left, right)
    def →(right: Vector): TwoPoints = ->(right)
  }

}
