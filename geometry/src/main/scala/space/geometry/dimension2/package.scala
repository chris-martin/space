package space.geometry

package object dimension2 extends Object with Approximations with ArrowOps {

  def xy(x: Double, y: Double): CartesianVector = CartesianVector(x, y)

  val ∠ = Angle
  val △ = Triangle
  val ○ = Circle

  implicit class DoubleEnrichedForDimension2(x: Double) {

    def radians: ArbitraryRadians = ArbitraryRadians(x)

  }

  type RotationDirection = Sign

  val Clockwise: RotationDirection = Negative

  val Counterclockwise: RotationDirection = Positive

}
