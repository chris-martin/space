package space.geometry

trait NearEqualityTesting {

  val nearEquality = NearEquality(.000000000001)
  import nearEquality._

  def ?[A](ok: Boolean, ifNot: => A): Option[A] =
    if (ok) None else Some(ifNot)

  implicit class ApproximateEqualizer[V: Distance](left: V) {

    def =~(right: V): Option[String] =
      ?(nearEqual(left, right), "%s did not equal %s" format (left, right))

    def !=~(right: V): Option[String] =
      ?(!nearEqual(left, right), "Both values equal %s" format left)

  }

}
