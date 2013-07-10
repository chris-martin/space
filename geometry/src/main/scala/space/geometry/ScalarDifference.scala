package space.geometry

trait ScalarDifference[-X] {

  def distance(a: X, b: X): Double

}
