package space.geometry
package dimension2
package line

import angle._
import vector._

class TwoPointLineSpec extends org.scalatest.FunSpec with NearEqualityTesting {

  val v = CartesianVector(_, _)

  it ("It calculates its angle") {
    assert ( TwoPointLine(v(2, 3), v(3, 4)).angle =~ Angle(math.Pi/4) )
  }

}
