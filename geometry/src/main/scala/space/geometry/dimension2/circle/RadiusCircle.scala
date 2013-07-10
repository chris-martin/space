package space.geometry
package dimension2
package circle

import vector._

sealed case class RadiusCircle(center: Vector, radius: Double) extends Circle {

  override def toRadiusCircle = this

}
