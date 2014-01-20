package space.color

import scala.math.abs

sealed case class RGB(red: ColorValue, green: ColorValue, blue: ColorValue)
extends Color with SpecificColorType[RGB] {

  private def max: Double = Seq(red, green, blue).max
  private def min: Double = Seq(red, green, blue).min
  private def chroma: Double = max - min

  override def toRGB: RGB = this

  def hue: Hue = if (chroma == 0) 0 else (
    if      (max == red.toDouble)   (green - blue)  / chroma
    else if (max == green.toDouble) (blue  - red)   / chroma + 2
    else                            (red   - green) / chroma + 4
  ) / 6

  override def toHSB: HSB = HSB(
    hue = hue,
    saturation = if (chroma == 0) 0 else chroma / max,
    brightness = max
  )

  override def toHSL: HSL = HSL(
    hue = hue,
    saturation = if (chroma == 0) 0 else chroma / (1 - abs(min + max - 1)),
    lightness = (min + max) / 2
  )
}

object RGB extends Object with ColorCompanion[RGB] {

  override def apply(v1: Double, v2: Double, v3: Double): RGB =
  apply(v1: ColorValue, v2: ColorValue, v3: ColorValue)
}

object RGBA extends TranslucentCompanion[RGB] {

  def OpaqueCompanion = RGB

  def apply(red: ColorValue, green: ColorValue, blue: ColorValue,
  alpha: ColorValue): RGBA = apply(RGB(red, green, blue), alpha)
}
