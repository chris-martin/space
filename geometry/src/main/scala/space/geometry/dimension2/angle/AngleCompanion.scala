package space.geometry.dimension2.angle

trait AngleCompanion[A] {

  def apply(a: Double): A

  def halfCircle: A = apply(Pi)
  def circle: A = apply(twoPi)

}
