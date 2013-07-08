package space.geometry

class NearEquality(val Ɛ: Double) {

  def nearEqual[V: Distance](left: V, right: V): Boolean =
    implicitly[Distance[V]].distance(left, right) < Ɛ

  implicit class RichVector[V: Distance](left: V) {
    def =~(right: V): Boolean = nearEqual(left, right)
    def !=~(right: V): Boolean = !nearEqual(left, right)
  }

}

object NearEquality {
  implicit def apply(Ɛ: Double): NearEquality = new NearEquality(Ɛ)
  implicit def unapply(epsilon: NearEquality): Double = epsilon.Ɛ
}
