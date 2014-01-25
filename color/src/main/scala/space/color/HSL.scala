package space.color

import math.abs

sealed case class HSL(hue: Hue, saturation: ColorValue,
    lightness: ColorValue) extends Color with SpecificColorType[HSL] {

  private def chroma: Double = saturation * (1 - abs(2 * lightness - 1))

  override def toHSL: HSL = this

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

    val m: Double = lightness - chroma / 2

    RGB(rgb._1 + m, rgb._2 + m, rgb._3 + m)
  }

  override def toHSB: HSB = toRGB.toHSB
}

object HSL extends ColorCompanion[HSL] {

  override def apply(v1: Double, v2: Double, v3: Double): HSL =
    apply(v1: Hue, v2: ColorValue, v3: ColorValue)
}

object HSLA extends TranslucentCompanion[HSL] {

  override def OpaqueCompanion = HSL

  def apply(hue: ColorValue, saturation: ColorValue,
      brightness: ColorValue, alpha: ColorValue): HSLA =
    apply(HSL(hue, saturation, brightness), alpha)
}
