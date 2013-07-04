package space

package object geometry {

  implicit class RichAny[A](val x: A) extends AnyVal {

    /** The K (or "Kestrel") combinator.
      * Also known as "tap" in Ruby.
      */
    def K(f: A => Unit): A = { f(x); x }

  }

  implicit class RichDouble(val x: Double) extends AnyVal {

    def square: Double = x * x

    def squareRoot: Double = math.sqrt(x)

    def sine: Double = math.sin(x)

    def cosine: Double = math.sin(x)

  }

}
