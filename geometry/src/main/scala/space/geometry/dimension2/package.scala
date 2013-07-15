package space.geometry

package object dimension2 extends Approximations {

  def xy(x: Double, y: Double): CartesianVector = CartesianVector(x, y)

  val ∠ = Angle

  implicit class DoubleEnrichedForDimension2(x: Double) {

    def radians: ArbitraryRadians = ArbitraryRadians(x)

  }

  type RotationDirection = Sign

  val Clockwise: RotationDirection = Negative

  val Counterclockwise: RotationDirection = Positive

  implicit class VectorArrowAssoc(left: Vector) {

    def ->(right: Vector): TwoPoints = TwoPoints(left, right)

    def →(right: Vector): TwoPoints = ->(right)

  }

  implicit class TwoPointsArrowAssoc(left: TwoPoints) {

    def ->(right: Vector): ThreePoints =
    ThreePoints(left.source, left.destination, right)

    def →(right: Vector): ThreePoints = ->(right)

  }

}
