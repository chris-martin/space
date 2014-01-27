package space.geometry
package dimension2

class AngleSpec extends org.scalatest.FreeSpec
    with space.approximation.ApproximationAssertions {

  "Angle from three points" - {

    "1" in assert( Radians(xy(0, 1), Origin, xy(1, 0)) ==~ Radians(Pi/2) )
    "2" in assert( Radians(xy(0, 1), xy(1, 0), xy(3, 0)) ==~ Radians(3*Pi/4) )
  }

  "AnyRadians" - {

    val a = AnyRadians(Pi/4)

    "It equals an identical AnyRadians" in
      assert ( a ==~ AnyRadians(Pi/4) )

    "It does not equal a different AnyRadians." in
      assert ( a !==~ AnyRadians(Pi/5) )

    "It does not equal itself plus 2 Pi." in
      assert ( a !==~ a + AnyRadians(2*Pi) )

    "It does not equal itself minus 2 Pi." in
      assert ( a !==~ a - AnyRadians(2*Pi) )
  }

  "CircleRadians" - {

    val a = CircleRadians(Pi/4)

    "It equals an identical CircleRadians." in
      assert ( a ==~ CircleRadians(Pi/4) )

    "It does not equal a different CircleRadians." in
      assert ( a !==~ CircleRadians(Pi/8) )

    "It equals itself plus a circle." in
      assert ( a ==~ a + Radians.circle )

    "It equals itself minus a circle." in
      assert ( a ==~ a - Radians.circle )
  }

  "SemicircleRadians" - {

    val a = SemicircleRadians(Pi/4)

    "It equals an identical SemicircleRadians." in
      assert ( a ==~ SemicircleRadians(Pi/4) )

    "It does not equal a different SemicircleRadians." in
      assert ( a !==~ SemicircleRadians(Pi/8) )

    "It equals itself plus a half circle." in
      assert ( a ==~ a + Radians.halfCircle )

    "It equals itself minus a half circle." in
      assert ( a ==~ a - Radians.halfCircle )
  }
}
