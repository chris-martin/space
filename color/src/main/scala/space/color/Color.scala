package space.color

trait Color { self =>

  def toRGB: RGB
  def toHSB: HSB
  def toHSL: HSL

  def withAlpha(alpha: ColorValue): Translucent[_] =
  Translucent(self, alpha)
}

trait SpecificColorType[C <: Color] { self: Color =>

  override def withAlpha(alpha: ColorValue): Translucent[C] =
  Translucent(self, alpha)
}

trait ColorCompanion[C <: Color] {

  def apply(v1: Double, v2: Double, v3: Double): C

  def apply(seq: Seq[ColorValue]): C = {
    require(seq.size == 3)
    apply(seq(0), seq(1), seq(2))
  }

  def hex(s: String): C = {

    val parse = s.length match {
      case 3 => parseHex1 _
      case 6 => parseHex2 _
      case _ => throw new IllegalArgumentException
    }

    val values = parse(s)

    apply(values)
  }
}
