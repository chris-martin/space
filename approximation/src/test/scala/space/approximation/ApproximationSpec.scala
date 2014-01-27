package space.approximation

class ApproximationSpec extends org.scalatest.FreeSpec {

  implicit val t: Tolerance = 0.0001

  "It can be used by mixing in the ApproximationOps trait" in
    new Object with ApproximationOps {
      assert( 5.4 =~ 5.400000001 )
      assert( 5.4 !=~ 5.41 )
    }

  "It can be used by importing ApproximationOps._" in {
    import ApproximationOps._
    assert( 5.4 =~ 5.400000001 )
    assert( 5.4 !=~ 5.41 )
  }
}
