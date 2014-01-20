package space.color

import scala.math.abs

sealed case class HSB(hue: Hue, saturation: ColorValue,
brightness: ColorValue) extends Color with SpecificColorType[HSB] {

  private def chroma: Double = brightness * saturation

  override def toHSB: HSB = this

  override def toRGB: RGB = {

    val h: Double = hue * 6

    val x: Double = chroma * ( 1 - abs(h % 2 - 1) )

    val rgb: (Double, Double, Double) =
    if      (h < 1) (chroma, x, 0)
    else if (h < 2) (x, chroma, 0)
    else if (h < 3) (0, chroma, x)
    else if (h < 4) (0, x, chroma)
    else if (h < 5) (x, 0, chroma)
    else            (chroma, 0, x)

    val m: Double = brightness - chroma

    RGB(rgb._1 + m, rgb._2 + m, rgb._3 + m)
  }

  override def toHSL: HSL = toRGB.toHSL
}

object HSB extends ColorCompanion[HSB] {

  override def apply(v1: Double, v2: Double, v3: Double): HSB =
  apply(v1: Hue, v2: ColorValue, v3: ColorValue)
}

object HSBA extends TranslucentCompanion[HSB] {

  def OpaqueCompanion = HSB

  def apply(hue: ColorValue, saturation: ColorValue, brightness: ColorValue,
  alpha: ColorValue): HSBA = apply(HSB(hue, saturation, brightness), alpha)
}
