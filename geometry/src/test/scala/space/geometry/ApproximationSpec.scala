package space.geometry

class ApproximationSpec extends org.scalatest.FunSpec {

  implicit val t: Tolerance = 0.0001

  it ("It can be used by mixing in the ImplicitApproximationOperator trait") {
    new Object with ImplicitApproximationOperator {
      assert( 5.4 =~ 5.400000001 )
      assert( 5.4 !=~ 5.41 )
    }
  }

  it ("It can be used by importing Approximation._") {
    import Approximation._
    assert( 5.4 =~ 5.400000001 )
    assert( 5.4 !=~ 5.41 )
  }

}
