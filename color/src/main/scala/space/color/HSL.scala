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
    if      (0 until 1 contains h) (chroma, x, 0)
    else if (1 until 2 contains h) (x, chroma, 0)
    else if (2 until 3 contains h) (0, chroma, x)
    else if (3 until 4 contains h) (0, x, chroma)
    else if (4 until 5 contains h) (x, 0, chroma)
    else                           (chroma, 0, x)

    val m: Double = lightness - chroma / 2

    RGB(rgb._1 + m, rgb._2 + m, rgb._3 + m)
  }

  override def toHSB: HSB = toRGB.toHSB
}

object HSL extends ColorCompanion[HSL] {

  override def apply(v1: Double, v2: Double, v3: Double) =
    apply(v1: Hue, v2: ColorValue, v3: ColorValue)
}

object HSLA extends TranslucentCompanion[HSL] {

  def OpaqueCompanion = HSL

  def apply(hue: ColorValue, saturation: ColorValue, brightness: ColorValue,
  alpha: ColorValue): HSLA = apply(HSL(hue, saturation, brightness), alpha)
}
