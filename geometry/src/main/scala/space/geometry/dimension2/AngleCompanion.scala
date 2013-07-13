package space.geometry
package dimension2

trait AngleCompanion[A] {

  def apply(a: Double): A

  def halfCircle: A = apply(Pi)
  def circle: A = apply(twoPi)

}
