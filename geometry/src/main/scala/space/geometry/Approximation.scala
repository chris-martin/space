package space.geometry

trait Approximation[A] extends Object with ImplicitApproximationOperator {

  def apply(a: A, b: A)(implicit tolerance: Tolerance): Boolean

  protected def $[X, Y](a: X, b: X)(f: X => Y)(implicit approximation:
  Approximation[Y], tolerance: Tolerance): Boolean = f(a) =~ f(b)

}

object Approximation extends ImplicitApproximationOperator

trait ImplicitApproximationOperator {

  implicit def anyToApproximationOperator[A](a: A)(implicit approximation:
  Approximation[A], tolerance: Tolerance): ApproximationOperator[A] =
  new ApproximationOperator[A](a)

}

class ApproximationOperator[A](left: A)(implicit approximation:
Approximation[A], tolerance: Tolerance) {

  def  =~(right: A): Boolean =  approximation(left, right)
  def !=~(right: A): Boolean = !approximation(left, right)

}
