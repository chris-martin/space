package space.geometry

class Tolerance(val Ɛ: Double) extends AnyVal

object Tolerance {

  implicit def apply(Ɛ: Double = .000000000001): Tolerance = new Tolerance(Ɛ)

  implicit def unapply(tolerance: Tolerance): Double = tolerance.Ɛ

}
