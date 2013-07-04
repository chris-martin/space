package space.geometry.dimension2

sealed trait Angular extends Any {

  def toDouble: Double

  def sine: Double = math.sin(toDouble)

  def cosine: Double = math.cos(toDouble)

}

class Radians (val toDouble: Double) extends AnyVal with Angular

object Radians {

  def apply(a: Double): Radians = new Radians(a)

}

/**
 * @param toDouble in the range (-Pi, Pi]
 */
class Angle private (val toDouble: Double) extends AnyVal with Angular

object Angle {

  import math.Pi

  val twoPi = 2 * Pi

  def apply(a: Double): Angle = new Angle(
    a % twoPi match {
      case b if b >   Pi => b - twoPi
      case b if b <= -Pi => b + twoPi
      case b             => b
    }
  )

}
