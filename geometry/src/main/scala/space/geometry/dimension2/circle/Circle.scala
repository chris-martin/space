package space.geometry
package dimension2
package circle

trait Circle {

  def center: Vector

  def radius: Double

  def toRadiusCircle: RadiusCircle

}
