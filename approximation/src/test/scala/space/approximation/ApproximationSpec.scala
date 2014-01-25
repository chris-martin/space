package space.approximation

class ApproximationSpec extends org.scalatest.FreeSpec {

  implicit val t: Tolerance = 0.0001

  "It can be used by mixing in the ImplicitApproximationOperator trait" in
    new Object with ImplicitApproximationOperator {
      assert( 5.4 =~ 5.400000001 )
      assert( 5.4 !=~ 5.41 )
    }

  "It can be used by importing Approximation._" in {
    import Approximation._
    assert( 5.4 =~ 5.400000001 )
    assert( 5.4 !=~ 5.41 )
  }
}
