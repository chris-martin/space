package space

package object color extends Object with Approximations {

  type HSLA = Translucent[HSL]
  type HSBA = Translucent[HSB]
  type RGBA = Translucent[RGB]

  implicit object ColorValueOrdering extends Ordering[ColorValue] {

    override def compare(x: ColorValue, y: ColorValue): Int =
    Ordering.Double.compare(x, y)
  }

  private[color] def parseHex1(s: String): Seq[ColorValue] =
  s map { c => ColorValue(BigInt(c.toString, 16).toDouble / 15) }

  private[color] def parseHex2(s: String): Seq[ColorValue] =
  s.grouped(2).toSeq map { c => ColorValue(BigInt(c.mkString, 16).toDouble / 255) }
}
