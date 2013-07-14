package space.geometry

trait ApproximationTesting {

  implicit val tolerance = Tolerance()

  implicit class Okay(ok: Boolean) {

    def ?[A](ifNot: => A): Option[A] =
      if (ok) None else Some(ifNot)

  }

  implicit class ApproximateEqualizer[A](left: A)
    (implicit approximation: Approximation[A]) {

    def =~(right: A): Option[String] =
      approximation(left, right) ? "%s is not equal to %s".format(left, right)

    def !=~(right: A): Option[String] =
      !approximation(left, right) ? "%s is equal to %s".format(left, right)

  }

}
