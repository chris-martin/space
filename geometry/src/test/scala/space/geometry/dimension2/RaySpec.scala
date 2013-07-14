package space.geometry
package dimension2

class RaySpec extends org.scalatest.FunSpec with ApproximationTesting {

  it ("It can be converted to a ray segment of magnitude 1") {
    val ray = Ray(CartesianVector(3, 2), CircleRadians(-Pi / 2))
    val unit = ray.unitSegment

    assert ( unit.length =~ 1 )

    assert (
      unit.toTwoPoints
      =~ TwoPoints(CartesianVector(3, 2), CartesianVector(3, 1))
    )
  }

  it ("It can be rotated about its source point") {
    assert (
      Ray(CartesianVector(4, 5), Angle(Pi/4)).rotate(Angle(Pi/4))
      =~ Ray(CartesianVector(4, 5), Angle(Pi/2))
    )
  }

}
