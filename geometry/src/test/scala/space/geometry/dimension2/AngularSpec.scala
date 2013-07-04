package space.geometry
package dimension2

import org.scalatest.FunSpec

import math.Pi

class AngularSpec extends FunSpec {

  describe("An Angle") {

    val a = Angle(Pi/4)

    it ("equals an identical angle") { assert ( a == Angle(Pi/4) ) }
    it ("equals itself plus 2 Pi") { assert ( a == a + Angle(2*Pi) ) }
    it ("equals itself minus 2 Pi") { assert ( a == a - Angle(2*Pi) ) }
    it ("does not equal a different angle") { assert ( a != Angle(Pi/8) ) }
  }

  describe("A Radians") {

    val a = Radians(Pi/4)

    it ("equals an identical radians") { assert ( a == Radians(Pi/4) ) }
    it ("does not equal itself plus 2 Pi") { assert ( a != a + Radians(2*Pi) ) }
  }

}
