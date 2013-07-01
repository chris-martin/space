package space

package object geometry {

  implicit class RichAny[A](x: A) {

    /** The K (or "Kestrel") combinator.
      * Also known as "tap" in Ruby.
      */
    def K(f: A => Unit): A = { f(x); x }

  }

}
