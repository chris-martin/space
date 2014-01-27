package space.approximation

/** Import `ApproximationOps._` or extend `ApproximationOps` to provide
  * approximate equivalence operators `=~` ("approximately equal")
  * and `!=~` ("not approximately equal") on every type `A` for which
  * there exists an implicit `Approximation[A]`.
  */
trait ApproximationOps {

  implicit class Op[A](left: A)(implicit approximation: Approximation[A],
      tolerance: Tolerance) {

    def  =~(right: A): Boolean =  approximation(left, right)

    def !=~(right: A): Boolean = !approximation(left, right)
  }

  implicit def optionToApproximationOperator[A](option: Option[A])
      (implicit approximation: Approximation[A], tolerance: Tolerance):
      Op[Option[A]] = {

    implicit val optionalApproximation = new OptionalApproximation[A]

    new Op(option)
  }
}

object ApproximationOps extends ApproximationOps
