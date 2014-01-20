package space.color

sealed class ColorValue private (val toDouble: Double) {

  override def toString = s"ColorValue($toDouble)"
}

object ColorValue {

  implicit def apply(x: Double): ColorValue =
  new ColorValue( if (x <= 0) 0 else if (x >= 1) 1 else x )

  implicit def toDouble(c: ColorValue): Double = c.toDouble
}
