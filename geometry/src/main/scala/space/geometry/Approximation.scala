package space.geometry

trait Approximation[A] extends ImplicitApproximationOperator {

  def apply(a: A, b: A)(implicit tolerance: Tolerance): Boolean

  protected def $[X, Y](a: X, b: X)(f: X => Y)(implicit approximation:
  Approximation[Y], tolerance: Tolerance): Boolean = f(a) =~ f(b)

}

object Approximation extends ImplicitApproximationOperator

trait ImplicitApproximationOperator {

  implicit class ApproximationOperator[A](left: A)(implicit approximation:
  Approximation[A], tolerance: Tolerance) {

    def  =~(right: A): Boolean =  approximation(left, right)

    def !=~(right: A): Boolean = !approximation(left, right)

  }

}
