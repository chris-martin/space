package space.color

sealed class Hue private (val toDouble: Double) {

  override def toString = s"Hue($toDouble)"
}

object Hue {

  implicit def apply(x: Double): Hue = new Hue({
    var h = x % 1
    if (h < 0) h += 1
    h
  })

  implicit def toDouble(h: Hue): Double = h.toDouble
}
