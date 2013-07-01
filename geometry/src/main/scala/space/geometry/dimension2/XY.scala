package space.geometry
package dimension2

/** A two-dimensional vector defined by X and Y coordinates.
  */
sealed class XY[Scalar: IsScalar](val x: Scalar, val y: Scalar,
    private var magnitudeOption: Option[Scalar] = None
  ) extends Vec[Scalar] {

  private val isScalar = implicitly[IsScalar[Scalar]]
  import isScalar._

  def xy: XY[Scalar] = this

  /** This result is memoized (its computation cost is incurred at most once).
    */
  override def magnitude: Scalar = magnitudeOption.getOrElse {
    (x.square + y.square).squareRoot K { m => magnitudeOption = Some(m) }
  }

  override def unary_- : XY[Scalar] = XY ( x * (-1: Scalar), y * (-1: Scalar) )

  override def +(that: Vec[Scalar]) = XY ( x + that.x, y + that.y )
  override def -(that: Vec[Scalar]) = XY ( x - that.x, y - that.y )

  def *(s: Scalar): XY[Scalar] = new XY ( x*s, y*s )
  def /(s: Scalar): XY[Scalar] = new XY ( x/s, y/s )

  override def equals(obj: Any): Boolean = obj match {
    case that: AnyRef if this eq that => true
    case that: Vec[_] => x == that.x && y == that.y
    case _ => false
  }

  override def hashCode() = List(x, y).hashCode()

  override def toString: String = "XY(%f, %f)" format (x, y)

}

object XY {

  def apply[Scalar: IsScalar](x: Scalar, y: Scalar): XY[Scalar] = new XY(x, y)

}
