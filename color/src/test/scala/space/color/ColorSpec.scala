package space.color

class ColorSpec extends org.scalatest.FreeSpec
with space.approximation.testing.ApproximationTesting {

  "Conversion" - {

    def conversions(name: String, rgb: RGB, hsb: HSB, hsl: HSL) {
      name - {
        "RGB to HSB" in assert(rgb.toHSB =~ hsb)
        "RGB to HSL" in assert(rgb.toHSL =~ hsl)
        "HSB to RGB" in assert(hsb.toRGB =~ rgb)
        "HSB to HSL" in assert(hsb.toHSL =~ hsl)
        "HSL to RGB" in assert(hsl.toRGB =~ rgb)
        "HSL to HSB" in assert(hsl.toHSB =~ hsb)
      }
    }

    conversions( "Black", RGB(0, 0, 0), HSB(0, 0, 0), HSL(0, 0, 0 ) )
    conversions( "White", RGB(1, 1, 1), HSB(0, 0, 1), HSL(0, 0, 1 ) )
    conversions( "Gray", RGB(.5, .5, .5), HSB(0, 0, .5), HSL(0, 0, .5 ) )

    conversions( "Red", RGB(1, 0, 0), HSB(0, 1, 1), HSL(0, 1, .5) )
    conversions( "Yellow", RGB(1, 1, 0), HSB(1d/6, 1, 1), HSL(1d/6, 1, .5) )
    conversions( "Green", RGB(0, 1, 0), HSB(1d/3, 1, 1), HSL(1d/3, 1, .5) )
    conversions( "Cyan", RGB(0, 1, 1), HSB(1d/2, 1, 1), HSL(1d/2, 1, .5) )
    conversions( "Blue", RGB(0, 0, 1), HSB(2d/3, 1, 1), HSL(2d/3, 1, .5) )
    conversions( "Magenta", RGB(1, 0, 1), HSB(5d/6, 1, 1), HSL(5d/6, 1, .5) )

    conversions( "Pink", RGB(1, .5, .5), HSB(0, .5, 1), HSL(0, 1, .75) )
  }

  "Hex parsing" - {

    "RGB" - {
      "short" in assert ( RGB.hex("f10")       =~ RGB(1, 1d/15, 0)      )
      "long"  in assert ( RGB.hex("ff0100")    =~ RGB(1, 1d/255, 0)     )
    }

    "RGBA" - {
      "short" in assert ( RGBA.hex("f0f1")     =~ RGBA(1, 0, 1, 1d/15)  )
      "long"  in assert ( RGBA.hex("ff00ff01") =~ RGBA(1, 0, 1, 1d/255) )
    }

    "HSL" - {
      "short" in assert ( HSL.hex("f10")       =~ HSL(1, 1d/15, 0)      )
      "long"  in assert ( HSL.hex("ff0100")    =~ HSL(1, 1d/255, 0)     )
    }

    "HSLA" - {
      "short" in assert ( HSLA.hex("f0f1")     =~ HSLA(1, 0, 1, 1d/15)  )
      "long"  in assert ( HSLA.hex("ff00ff01") =~ HSLA(1, 0, 1, 1d/255) )
    }

    "HSB" - {
      "short" in assert ( HSB.hex("f10")       =~ HSB(1, 1d/15, 0)      )
      "long"  in assert ( HSB.hex("ff0100")    =~ HSB(1, 1d/255, 0)     )
    }

    "HSBA" - {
      "short" in assert ( HSBA.hex("f0f1")     =~ HSBA(1, 0, 1, 1d/15)  )
      "long"  in assert ( HSBA.hex("ff00ff01") =~ HSBA(1, 0, 1, 1d/255) )
    }
  }
}
