package space.geometry
package dimension2
package angle

trait Angle[A <: Angle[A]] extends Any {

  def toDouble: Double

  def sine: Double = math.sin(toDouble)
  def cosine: Double = math.cos(toDouble)

  protected def companion: AngleCompanion[A]

  def +(that: A): A = companion(toDouble + that.toDouble)
  def -(that: A): A = companion(toDouble - that.toDouble)

  def *(s: Double): A = companion(toDouble * s)
  def /(s: Double): A = companion(toDouble / s)

}

object Angle extends ArbitraryRadiansCompanion
