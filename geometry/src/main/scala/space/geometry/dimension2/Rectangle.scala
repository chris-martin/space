package space.geometry
package dimension2

trait Rectangle {

  def center: Point

  def arbitraryDiagonal: LineSegment

  def arbitraryDiagonalAndCornerRectangle: DiagonalAndCornerRectangle

  /** Rotation about the center.
    */
  def rotate(angle: AnyRadians): Rectangle

  /** Rotation about a pivot.
    */
  def rotate(angle: AnyRadians, pivot: Point): Rectangle

  def pad(padding: Double): Rectangle

  def perimeter: Rectangle.Perimeter

  def interior: Rectangle.Interior

  def exterior: Rectangle.Exterior
}

object Rectangle {

  trait Has {

    def rectangle: Rectangle
  }

  trait Perimeter extends Has {

    def length: Double

    override def toString: String = s"Rectangle.Perimeter($rectangle)"
  }

  trait Interior extends Has {

    def area: Double

    /** The interior of a rectangle that has the same center and proportions
      * as this one, scaled to an area of `newArea`.
      */
    def withArea(newArea: Double): Interior

    /** The interior of a rectangle that has the same center and proportions
      * as this one, scaled to an area of `area + additionalArea`.
      */
    def +(additionalArea: Double): Interior = withArea(area + additionalArea)

    /** The interior of a rectangle that has the same center and proportions
      * as this one, scaled to an area of `area + removedArea`.
      */
    def -(removedArea: Double): Interior = withArea(area - removedArea)

    /** The interior of a rectangle that has the same center and proportions
      * as this one, scaled to an area of `area * areaFactor`.
      */
    def *(areaFactor: Double): Interior = withArea(area * areaFactor)

    /** The interior of a rectangle that has the same center and proportions
      * as this one, scaled to an area of `area / areaDivisor`.
      */
    def /(areaDivisor: Double): Interior = withArea(area / areaDivisor)

    override def toString: String = s"Rectangle.Interior($rectangle)"
  }

  trait Exterior extends Has {

    override def toString: String = s"Rectangle.Exterior($rectangle)"
  }
}

sealed case class DiagonalAndCornerRectangle(
    diagonal: LineSegment, corner: Point) extends Rectangle {

  override def center: Point = diagonal.midpoint

  override def arbitraryDiagonal: LineSegment = diagonal

  override def arbitraryDiagonalAndCornerRectangle:
    DiagonalAndCornerRectangle = this

  override def rotate(angle: AnyRadians): DiagonalAndCornerRectangle =
    DiagonalAndCornerRectangle(
      diagonal = diagonal.rotate(angle),
      corner = corner.rotate(angle, center)
    )

  override def rotate(angle: AnyRadians, pivot: Point):
      DiagonalAndCornerRectangle =
    DiagonalAndCornerRectangle(
      diagonal = diagonal.rotate(angle, pivot),
      corner = corner.rotate(angle, pivot)
    )

  def otherCorner: Point = corner rotate (Radians.halfCircle, center)

  def flipCorner: DiagonalAndCornerRectangle =
    DiagonalAndCornerRectangle(diagonal, otherCorner)

  override def pad(padding: Double): DiagonalAndCornerRectangle = ???

  override val perimeter = DiagonalAndCornerRectangle.Perimeter(this)

  override val interior = DiagonalAndCornerRectangle.Interior(this)

  override val exterior = DiagonalAndCornerRectangle.Exterior(this)
}

object DiagonalAndCornerRectangle {

  sealed case class Perimeter(rectangle: DiagonalAndCornerRectangle)
      extends Rectangle.Perimeter {

    import rectangle._

    override def length: Double = 2 *
      diagonal.arbitrarilyDirected.toSeq.map(p => (p → corner).length).sum
  }

  sealed case class Interior(rectangle: DiagonalAndCornerRectangle)
      extends Rectangle.Interior {

    import rectangle._

    override def area: Double = {
      val d = diagonal.arbitrarilyDirected
      val triangle = Triangle(d.source, d.destination, corner)
      2 * triangle.interior.area
    }

    override def withArea(newArea: Double):
        DiagonalAndCornerRectangle.Interior = {

      val (a, b) = {
        val (a, b) = diagonal.arbitrarilyDirected.toTuple
        ((a → corner).length, (b → corner).length)
      }

      val newDiagonalLength = sqrt(newArea * (a/b + b/a))

      DiagonalAndCornerRectangle(
        diagonal = diagonal withLength newDiagonalLength,
        corner = (flipCorner.diagonal withLength newDiagonalLength)
        .arbitrarilyDirected.source
      ).interior
    }
  }

  sealed case class Exterior(rectangle: DiagonalAndCornerRectangle)
      extends Rectangle.Exterior
}

sealed case class OrthogonalRectangle(center: Point, size: CartesianVector)
    extends Rectangle {

  require(size.x >= 0)
  require(size.y >= 0)

  private lazy val dx = size.x / 2
  private lazy val dy = size.y / 2

  object bound extends OrthogonalBoundingBox {
    override def min: CartesianVector = xy( center.x - dx, center.y - dy )
    override def max: CartesianVector = xy( center.x + dx, center.y + dy )
  }

  override def arbitraryDiagonal: LineSegment = {
    import bound._
    LineSegment( xy(min.x, min.y), xy(max.x, max.y) )
  }

  override def arbitraryDiagonalAndCornerRectangle:
      DiagonalAndCornerRectangle = {
    import bound._
    DiagonalAndCornerRectangle(
      diagonal = LineSegment( xy(min.x, min.y), xy(max.x, max.y) ),
      corner = xy(min.x, max.y)
    )
  }

  override def rotate(angle: AnyRadians): RotatedOrthogonalRectangle =
    RotatedOrthogonalRectangle(this, angle)

  override def rotate(angle: AnyRadians, pivot: Point): Rectangle =
    arbitraryDiagonalAndCornerRectangle.rotate(angle, pivot)

  override def pad(padding: Double): OrthogonalRectangle =
    copy(size = size + padding * xy(2, 2))

  override val perimeter = OrthogonalRectangle.Perimeter(this)

  override val interior = OrthogonalRectangle.Interior(this)

  override val exterior = OrthogonalRectangle.Exterior(this)
}

object OrthogonalRectangle {

  def apply(c1: Point, c2: Point): OrthogonalRectangle =
    OrthogonalRectangle(
      center = (c1 → c2).midpoint,
      size = xy(
        math.abs(c1.x - c2.x),
        math.abs(c1.y - c2.y)
      )
    )

  sealed case class Perimeter(rectangle: OrthogonalRectangle)
      extends Rectangle.Perimeter {

    import rectangle._

    override def length = 2 * (size.x + size.y)

    def bottom: LineSegment = bound.edge.bottom
    def top: LineSegment = bound.edge.top
    def left: LineSegment = bound.edge.left
    def right: LineSegment = bound.edge.right
  }

  sealed case class Interior(rectangle: OrthogonalRectangle)
      extends Rectangle.Interior {

    import rectangle._

    override def area: Double = size.x * size.y

    override def withArea(newArea: Double): OrthogonalRectangle.Interior = {
      val newSizeX = sqrt( size.x * newArea / size.y )
      val newSizeY = newArea / newSizeX
      rectangle.copy( size = xy(newSizeX, newSizeY) ).interior
    }
  }

  sealed case class Exterior(rectangle: OrthogonalRectangle)
      extends Rectangle.Exterior
}

sealed case class RotatedOrthogonalRectangle(
    orthogonal: OrthogonalRectangle, angle: SemicircleRadians)
    extends Rectangle {

  override def center: Point = orthogonal.center

  override def arbitraryDiagonal: LineSegment =
    orthogonal.arbitraryDiagonal.rotate(angle.toAnyRadiansArbitrarily)

  override def arbitraryDiagonalAndCornerRectangle:
      DiagonalAndCornerRectangle =
    orthogonal.arbitraryDiagonalAndCornerRectangle
      .rotate(angle.toAnyRadiansArbitrarily)

  override def rotate(angle: AnyRadians): RotatedOrthogonalRectangle =
    RotatedOrthogonalRectangle(orthogonal, this.angle + angle)

  override def rotate(angle: AnyRadians, pivot: Point): Rectangle =
    arbitraryDiagonalAndCornerRectangle.rotate(angle, pivot)

  override def pad(padding: Double): RotatedOrthogonalRectangle =
    copy(orthogonal = orthogonal.pad(padding))

  override val perimeter = RotatedOrthogonalRectangle.Perimeter(this)

  override val interior = RotatedOrthogonalRectangle.Interior(this)

  override val exterior = RotatedOrthogonalRectangle.Exterior(this)
}

object RotatedOrthogonalRectangle {

  sealed case class Perimeter(rectangle: RotatedOrthogonalRectangle)
      extends Rectangle.Perimeter {

    import rectangle._

    override def length = orthogonal.perimeter.length
  }

  sealed case class Interior(rectangle: RotatedOrthogonalRectangle)
      extends Rectangle.Interior {

    import rectangle._

    override def area: Double = orthogonal.interior.area

    override def withArea(newArea: Double):
        RotatedOrthogonalRectangle.Interior =
      rectangle.copy(
        orthogonal = orthogonal.interior.withArea(newArea).rectangle
      ).interior
  }

  sealed case class Exterior(rectangle: RotatedOrthogonalRectangle)
      extends Rectangle.Exterior
}
