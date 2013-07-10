package space.geometry
package dimension2
package angle

trait AngleCompanion[A] {

  def apply(a: Double): A

  def halfCircle: A = apply(Pi)
  def circle: A = apply(twoPi)

}
