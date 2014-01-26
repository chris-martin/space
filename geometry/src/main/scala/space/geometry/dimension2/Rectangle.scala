package space.geometry
package dimension2

trait Rectangle {

  def center: Point

  def arbitraryDiagonal: LineSegment

  def arbitraryDiagonalAndCorner: DiagonalAndCornerRectangle

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
    def area(newArea: Double): Interior

    /** The interior of a rectangle that has the same center and proportions
      * as this one, scaled to an area of `area + additionalArea`.
      */
    def +(additionalArea: Double): Interior = area(area + additionalArea)

    /** The interior of a rectangle that has the same center and proportions
      * as this one, scaled to an area of `area + removedArea`.
      */
    def -(removedArea: Double): Interior = area(area - removedArea)

    /** The interior of a rectangle that has the same center and proportions
      * as this one, scaled to an area of `area * areaFactor`.
      */
    def *(areaFactor: Double): Interior = area(area * areaFactor)

    /** The interior of a rectangle that has the same center and proportions
      * as this one, scaled to an area of `area / areaDivisor`.
      */
    def /(areaDivisor: Double): Interior = area(area / areaDivisor)

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

  override def arbitraryDiagonalAndCorner:
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

    override def area(newArea: Double): DiagonalAndCornerRectangle.Interior = {

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

sealed case class OrthogonalRectangle(center: Point,
    sizeX: Double, sizeY: Double) extends Rectangle {

  require(sizeX >= 0)
  require(sizeY >= 0)

  private lazy val dx = sizeX / 2
  private lazy val dy = sizeY / 2

  override def arbitraryDiagonal: LineSegment =
    LineSegment(
      xy(center.x - dx, center.y - dy),
      xy(center.x + dy, center.y + dy)
    )

  override def arbitraryDiagonalAndCorner: DiagonalAndCornerRectangle =
    DiagonalAndCornerRectangle(
      diagonal = LineSegment(
        xy(center.x - dx, center.y - dy),
        xy(center.x + dy, center.y + dy)
      ),
      corner = xy(center.x - dx, center.y + dy)
    )

  override def rotate(angle: AnyRadians): RotatedOrthogonalRectangle =
    RotatedOrthogonalRectangle(this, angle)

  override def rotate(angle: AnyRadians, pivot: Point): Rectangle =
    arbitraryDiagonalAndCorner.rotate(angle, pivot)

  override def pad(padding: Double): OrthogonalRectangle =
    copy(
      sizeX = sizeX + 2*padding,
      sizeY = sizeY + 2*padding
    )

  override val perimeter = OrthogonalRectangle.Perimeter(this)

  override val interior = OrthogonalRectangle.Interior(this)

  override val exterior = OrthogonalRectangle.Exterior(this)
}

object OrthogonalRectangle {

  def apply(c1: Point, c2: Point): OrthogonalRectangle =
    OrthogonalRectangle(
      center = (c1->c2).midpoint,
      sizeX = math.abs(c1.x - c2.x),
      sizeY = math.abs(c1.y - c2.y)
    )

  sealed case class Perimeter(rectangle: OrthogonalRectangle)
      extends Rectangle.Perimeter {

    import rectangle._

    override def length = 2 * (sizeX + sizeY)

    def bottom: LineSegment = LineSegment(
      xy( center.x - dx, center.y - dy ),
      xy( center.x + dx, center.y - dy )
    )

    def top: LineSegment = LineSegment(
      xy( center.x - dx, center.y + dy ),
      xy( center.x + dx, center.y + dy )
    )

    def left: LineSegment = LineSegment(
      xy( center.x - dx, center.y + dy ),
      xy( center.x - dx, center.y - dy )
    )

    def right: LineSegment = LineSegment(
      xy( center.x + dx, center.y + dy ),
      xy( center.x + dx, center.y - dy )
    )
  }

  sealed case class Interior(rectangle: OrthogonalRectangle)
      extends Rectangle.Interior {

    import rectangle._

    override def area: Double = sizeX * sizeY

    override def area(newArea: Double): OrthogonalRectangle.Interior = {

      val newSizeX = sqrt( sizeX * newArea / sizeY )

      val newSizeY = newArea / newSizeX

      rectangle.copy( sizeX = newSizeX, sizeY = newSizeY ).interior
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

  override def arbitraryDiagonalAndCorner: DiagonalAndCornerRectangle =
    orthogonal.arbitraryDiagonalAndCorner.rotate(angle.toAnyRadiansArbitrarily)

  override def rotate(angle: AnyRadians): RotatedOrthogonalRectangle =
    RotatedOrthogonalRectangle(orthogonal, this.angle + angle)

  override def rotate(angle: AnyRadians, pivot: Point): Rectangle =
    arbitraryDiagonalAndCorner.rotate(angle, pivot)

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

    override def area(newArea: Double): RotatedOrthogonalRectangle.Interior =
      rectangle.copy(
        orthogonal = orthogonal.interior.area(newArea).rectangle
      ).interior
  }

  sealed case class Exterior(rectangle: RotatedOrthogonalRectangle)
      extends Rectangle.Exterior
}
