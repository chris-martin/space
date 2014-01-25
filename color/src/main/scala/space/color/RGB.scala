package space.color

import scala.math.abs

sealed case class RGB(red: ColorValue, green: ColorValue, blue: ColorValue)
    extends Color with SpecificColorType[RGB] {

  def values = Seq(red, green, blue)

  private def max: Double = values.max
  private def min: Double = values.min
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

  def toHexString: String = "#" + values.map({ v =>
    var s = BigInt((v*255).toInt).toString(16)
    if (s.length == 1) s = "0" + s
    s
  }).mkString
}

object RGB extends Object with ColorCompanion[RGB] {

  override def apply(v1: Double, v2: Double, v3: Double): RGB =
    apply(v1: ColorValue, v2: ColorValue, v3: ColorValue)
}

object RGBA extends TranslucentCompanion[RGB] {

  override def OpaqueCompanion = RGB

  def apply(red: ColorValue, green: ColorValue, blue: ColorValue,
    alpha: ColorValue): RGBA = apply(RGB(red, green, blue), alpha)
}
