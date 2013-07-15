package space.geometry

trait Approximation[A] extends Object with ImplicitApproximationOperator {

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

trait ImplicitApproximationOperator {

  implicit def anyToApproximationOperator[A](a: A)(implicit approximation:
  Approximation[A], tolerance: Tolerance): ApproximationOperator[A] =
  new ApproximationOperator(a)

  implicit def optionToApproximationOperator[A](option: Option[A])
  (implicit approximation: Approximation[A], tolerance: Tolerance):
  ApproximationOperator[Option[A]] = {

    implicit val optionalApproximation = new OptionalApproximation[A]

    new ApproximationOperator(option)

  }

}

class ApproximationOperator[A](left: A)(implicit approximation:
Approximation[A], tolerance: Tolerance) {

  def  =~(right: A): Boolean =  approximation(left, right)
  def !=~(right: A): Boolean = !approximation(left, right)

}
