package space.geometry
package dimension2

trait OrthogonalBoundingBox { box =>

  def min: CartesianVector
  def max: CartesianVector

  def left   : Double = min.x
  def right  : Double = max.x
  def bottom : Double = min.y
  def top    : Double = max.y

  object edge {

    def bottom: LineSegment =
      LineSegment(
        xy( box.left, box.bottom ),
        xy( box.right, box.bottom )
      )

    def top: LineSegment = LineSegment(
      xy( box.left, box.top ),
      xy( box.right, box.top )
    )

    def left: LineSegment = LineSegment(
      xy( box.left, box.top ),
      xy( box.left, box.bottom )
    )

    def right: LineSegment = LineSegment(
      xy( box.right, box.top ),
      xy( box.right, box.bottom )
    )
  }
}
