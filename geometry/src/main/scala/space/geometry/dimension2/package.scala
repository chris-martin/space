package space.geometry

package object dimension2 {

  implicit object VectorDistance extends ScalarDifference[Vector] {
    override def distance(a: Vector, b: Vector) = (a - b).magnitude
  }

}
