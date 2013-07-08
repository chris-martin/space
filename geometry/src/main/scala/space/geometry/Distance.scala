package space.geometry

trait Distance[-V] {

  def distance(a: V, b: V): Double

}
