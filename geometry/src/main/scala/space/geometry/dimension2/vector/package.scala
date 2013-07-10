package space.geometry
package dimension2

package object vector {

  implicit object VectorDistance extends ScalarDifference[vector.Vector] {

    override def distance(a: vector.Vector, b: vector.Vector) =
      (a - b).magnitude

  }

}
