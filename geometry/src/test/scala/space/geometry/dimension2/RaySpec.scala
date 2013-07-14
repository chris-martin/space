package space.geometry
package dimension2

class RaySpec extends org.scalatest.FunSpec with ApproximationTesting {

  it ("It can be converted to a ray segment of magnitude 1") {
    val ray = Ray(CartesianVector(3, 2), CircleRadians(-Pi / 2))
    val unit = ray.unitSegment
    assert ( unit.toTwoPoints =~ TwoPoints(CartesianVector(3, 2), CartesianVector(3, 1)) )
    assert ( unit.length =~ 1 )
  }

}
