package space.geometry
package dimension2
package circle

import vector._

trait Circle {

  def center: Vector

  def radius: Double

  def toRadiusCircle: RadiusCircle

}
