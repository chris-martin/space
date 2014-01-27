package space.approximation

class OptionalApproximation[A](implicit approximation: Approximation[A])
    extends Approximation[Option[A]] {

  override def apply(optionA: Option[A], optionB: Option[A])
      (implicit tolerance: Tolerance): Boolean =
    (optionA, optionB) match {
      case (Some(a), Some(b)) => a =~ b
      case (None, None) => true
      case _ => false
    }
}
