package space.approximation

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
