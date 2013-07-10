package space.geometry
package dimension2
package line

import vector._
import angle._

sealed case class TwoPointLine(a: Vector, b: Vector) extends Line {

  override def point: Vector = a

  override def angle: SemicircleRadians = (b-a).angle

}
