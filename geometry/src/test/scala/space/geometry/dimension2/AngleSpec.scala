package space.geometry
package dimension2

class AngleSpec extends org.scalatest.FreeSpec
with space.approximation.testing.ApproximationTesting {

  "ArbitraryRadians" - {

    val a = ArbitraryRadians(Pi/4)

    "It equals an identical ArbitraryRadians" in
    { assert ( a =~ ArbitraryRadians(Pi/4) ) }

    "It does not equal a different ArbitraryRadians." in
    { assert ( a !=~ ArbitraryRadians(Pi/5) ) }

    "It does not equal itself plus 2 Pi." in
    { assert ( a !=~ a + ArbitraryRadians(2*Pi) ) }

    "It does not equal itself minus 2 Pi." in
    { assert ( a !=~ a - ArbitraryRadians(2*Pi) ) }
  }

  "CircleRadians" - {

    val a = CircleRadians(Pi/4)

    "It equals an identical CircleRadians." in
    { assert ( a =~ CircleRadians(Pi/4) ) }

    "It does not equal a different CircleRadians." in
    { assert ( a !=~ CircleRadians(Pi/8) ) }

    "It equals itself plus a circle." in
    { assert ( a =~ a + Angle.circle ) }

    "It equals itself minus a circle." in
    { assert ( a =~ a - Angle.circle ) }
  }

  "SemicircleRadians" - {

    val a = SemicircleRadians(Pi/4)

    "It equals an identical SemicircleRadians." in
    { assert ( a =~ SemicircleRadians(Pi/4) ) }

    "It does not equal a different SemicircleRadians." in
    { assert ( a !=~ SemicircleRadians(Pi/8) ) }

    "It equals itself plus a half circle." in
    { assert ( a =~ a + Angle.halfCircle ) }

    "It equals itself minus a half circle." in
    { assert ( a =~ a - Angle.halfCircle ) }
  }

}
