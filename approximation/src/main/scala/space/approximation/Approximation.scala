package space.approximation

/** An `Approximation[A]` defines a criterion by which a pair of values of
  * type `A` may be considered approximately equal.
  */
trait Approximation[A] extends Object with ApproximationOps {

  type Tolerance = space.approximation.Tolerance

  /** True if the values `a` and `b` are approximately equal.
    */
  def apply(a: A, b: A)(implicit tolerance: Tolerance): Boolean

  /** `$(a, b)(f)` is a terse way to write `f(a) =~ f(b)`.
    *
    * This method name is obscure for the sake of brevity, which is
    * acceptable only because it is only intended to be used when defining
    * new `Approximation`s, which shouldn't happen much in application code.
    * There is no apparent meaningful name for it anyway.
    */
  protected def $[X, Y](a: X, b: X)(f: X => Y)(implicit approximation:
    Approximation[Y], tolerance: Tolerance): Boolean = f(a) =~ f(b)
}
