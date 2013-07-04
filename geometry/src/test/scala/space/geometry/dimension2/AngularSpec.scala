package space.geometry.dimension2

import org.scalatest.FunSpec

import math.Pi

class AngularSpec extends FunSpec {

  describe("Angle(Pi/4)") {

    val a = Angle(Pi/4)

    it ("equals Angle(Pi / 4)") { assert ( a == Angle(Pi/4) ) }
    it ("equals Angle(9 Pi / 4)") { assert ( a == Angle(9*Pi/4) ) }
    it ("equals Angle(-7 Pi / 4)") { assert ( a == Angle(-7*Pi/4) ) }
    it ("does not equal Angle(Pi / 8)") { assert ( a != Angle(Pi/8) ) }
  }

  describe("Radians(Pi/4)") {

    val a = Radians(Pi/4)

    it ("equals Radians(Pi / 4)") { assert ( a == Radians(Pi/4) ) }
    it ("does not equal Angle(9 Pi / 4)") { assert ( a != Radians(9*Pi/4) ) }
  }

}
