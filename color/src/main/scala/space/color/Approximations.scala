package space.color

import space.approximation.Approximation

trait Approximations {

  implicit object `ColorValue approximation`
      extends Approximation[ColorValue] {

    override def apply(a: ColorValue, b: ColorValue)
        (implicit tolerance: Tolerance): Boolean =
      $(a,b)(_.toDouble)
  }

  implicit object `Hue approximation` extends Approximation[Hue] {

    override def apply(a: Hue, b: Hue)
        (implicit tolerance: Tolerance): Boolean =
      Hue(a.toDouble - b.toDouble).toDouble < tolerance
  }

  implicit object `RGB approximation` extends Approximation[RGB] {

    override def apply(a: RGB, b: RGB)
        (implicit tolerance: Tolerance): Boolean =
      $(a,b)(_.red) && $(a,b)(_.green) && $(a,b)(_.blue)
  }

  implicit object `HSB approximation` extends Approximation[HSB] {

    override def apply(a: HSB, b: HSB)
        (implicit tolerance: Tolerance): Boolean =
      $(a,b)(_.hue) && $(a,b)(_.saturation) && $(a,b)(_.brightness)
  }

  implicit object `HSL approximation` extends Approximation[HSL] {

    override def apply(a: HSL, b: HSL)
        (implicit tolerance: Tolerance): Boolean =
      $(a,b)(_.hue) && $(a,b)(_.saturation) && $(a,b)(_.lightness)
  }

  implicit object `Color approximation` extends Approximation[Color] {

    override def apply(a: Color, b: Color)
        (implicit tolerance: Tolerance): Boolean = $(a,b)(_.toRGB)
  }

  trait `generic TranslucentColor approximation`[C <: Color]
      extends Approximation[Translucent[C]] {

    override def apply(a: Translucent[C], b: Translucent[C])
        (implicit tolerance: Tolerance): Boolean =
      $(a,b)(_.opaque) && $(a,b)(_.alpha)
  }

  implicit object `TranslucentColor approximation`
      extends `generic TranslucentColor approximation`[Color]

  implicit object `RGBA approximation`
      extends `generic TranslucentColor approximation`[RGB]

  implicit object `HSBA approximation`
      extends `generic TranslucentColor approximation`[HSB]

  implicit object `HSLA approximation`
      extends `generic TranslucentColor approximation`[HSL]
}
