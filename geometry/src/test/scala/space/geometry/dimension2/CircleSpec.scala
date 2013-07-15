package space.geometry
package dimension2

class CircleSpec extends org.scalatest.FreeSpec {

  "Circle" - {

    "It can be constructed as ○(_,_) or Circle(_,_)" in
    { val c = xy(3.5, 6); val r = Pi/7
      assert( ○(c, r) === Circle(c, r) ) }

  }

}
