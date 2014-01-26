package space.geometry

package object dimension2 extends Object with Approximations with SampleOps
    with ArrowOps {

  type Radians = GenRadians[_]

  def xy(x: Double, y: Double): CartesianVector = CartesianVector(x, y)

  val ∠ = Radians
  val △ = Triangle
  val ○ = Circle

  implicit class DoubleEnrichedForDimension2(x: Double) {

    def radians: AnyRadians = AnyRadians(x)

    def *(point: Point): Point = point * x
  }

  type RotationDirection = Sign

  val Clockwise: RotationDirection = Negative

  val Counterclockwise: RotationDirection = Positive
}
