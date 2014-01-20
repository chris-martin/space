package space.color

sealed case class Translucent[+C <: Color]
(opaque: Color, alpha: ColorValue)

trait TranslucentCompanion[C <: Color] {

  def apply(opaque: C, alpha: ColorValue): Translucent[C] =
  Translucent(opaque, alpha)

  def OpaqueCompanion: ColorCompanion[C]

  def hex(s: String): Translucent[C] = {

    val parse = s.length match {
      case 4 => parseHex1 _
      case 8 => parseHex2 _
      case _ => throw new IllegalArgumentException
    }

    val values = parse(s)

    Translucent(OpaqueCompanion(values take 3), values.last)
  }
}
