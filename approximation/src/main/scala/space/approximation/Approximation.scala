package space.approximation

trait Approximation[A] extends Object with ImplicitApproximationOperator {

  type Tolerance = space.approximation.Tolerance

  def apply(a: A, b: A)(implicit tolerance: Tolerance): Boolean

  protected def $[X, Y](a: X, b: X)(f: X => Y)(implicit approximation:
  Approximation[Y], tolerance: Tolerance): Boolean = f(a) =~ f(b)
}

object Approximation extends ImplicitApproximationOperator

class OptionalApproximation[A](implicit approximation: Approximation[A])
extends Approximation[Option[A]] {

  override def apply(optionA: Option[A], optionB: Option[A])
  (implicit tolerance: Tolerance): Boolean = (optionA, optionB) match {
    case (Some(a), Some(b)) => a =~ b
    case (None, None) => true
    case _ => false
  }
}
