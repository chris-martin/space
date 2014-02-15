package space.geometry
package dimension2

trait Quadrilateral extends Polygon {

  override def arbitraryPath: FourPoints

  override def perimeter: Quadrilateral.Perimeter
  override def interior: Quadrilateral.Interior
  override def exterior: Quadrilateral.Exterior
}

trait DefinedByQuadrilateral[+A <: Quadrilateral] extends DefinedByPolygon[A] {

  def polygon: A

  def quadrilateral: A = polygon
}

object Quadrilateral {

  trait PerimeterOf[+A <: Quadrilateral] extends Polygon.PerimeterOf[A]
      with DefinedByQuadrilateral[A] {

    override def toString: String = s"Quadrilateral.Perimeter($quadrilateral)"
  }

  trait InteriorOf[+A <: Quadrilateral] extends Polygon.InteriorOf[A]
      with DefinedByQuadrilateral[A] {

    override def toString: String = s"Quadrilateral.Interior($quadrilateral)"
  }

  trait ExteriorOf[+A <: Quadrilateral] extends Polygon.ExteriorOf[A]
      with DefinedByQuadrilateral[A] {

    override def toString: String = s"Quadrilateral.Exterior($quadrilateral)"
  }

  type Perimeter = PerimeterOf[Quadrilateral]
  type Interior = InteriorOf[Quadrilateral]
  type Exterior = ExteriorOf[Quadrilateral]
}

trait Rectangle extends Quadrilateral {

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

  /** The interior of a rectangle that has the same center and proportions
    * as this one, scaled to an area of `newArea`.
    */
  def withArea(newArea: Double): Rectangle

  override def perimeter: Rectangle.Perimeter
  override def interior: Rectangle.Interior
  override def exterior: Rectangle.Exterior
}

trait DefinedByRectangle[+A <: Rectangle] extends DefinedByQuadrilateral[A] {

  def polygon: A

  def rectangle: A = polygon
}

object Rectangle {

  trait PerimeterOf[+A <: Rectangle] extends Quadrilateral.PerimeterOf[A]
      with DefinedByRectangle[A] {

    override def toString: String = s"Rectangle.Perimeter($rectangle)"
  }

  trait InteriorOf[+A <: Rectangle] extends Quadrilateral.InteriorOf[A]
      with DefinedByRectangle[A] {

    override def toString: String = s"Rectangle.Interior($rectangle)"
  }

  trait ExteriorOf[+A <: Rectangle] extends Quadrilateral.ExteriorOf[A]
      with DefinedByRectangle[A] {

    override def toString: String = s"Rectangle.Exterior($rectangle)"
  }

  type Perimeter = PerimeterOf[Rectangle]
  type Interior = InteriorOf[Rectangle]
  type Exterior = ExteriorOf[Rectangle]
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

  override def arbitraryPath: FourPoints = {
    val d = diagonal.arbitrarilyDirected
    Cycle( d.source, corner, d.destination, otherCorner )
  }

  override def area: Double = {
    val d = diagonal.arbitrarilyDirected
    2 * Triangle(d.source, d.destination, corner).area
  }

  override def withArea(newArea: Double): DiagonalAndCornerRectangle = {

    val (a, b) = {
      val (a, b) = diagonal.arbitrarilyDirected.toTuple
      ((a → corner).length, (b → corner).length)
    }

    val newDiagonalLength = sqrt(newArea * (a/b + b/a))

    DiagonalAndCornerRectangle(
      diagonal = diagonal withLength newDiagonalLength,
      corner = (flipCorner.diagonal withLength newDiagonalLength)
        .arbitrarilyDirected.source
    )
  }

  override def perimeter = DiagonalAndCornerRectangle.Perimeter(this)
  override def interior = DiagonalAndCornerRectangle.Interior(this)
  override def exterior = DiagonalAndCornerRectangle.Exterior(this)
}

object DiagonalAndCornerRectangle {

  sealed case class Perimeter(polygon: DiagonalAndCornerRectangle)
      extends Rectangle.PerimeterOf[DiagonalAndCornerRectangle] {

    override def length: Double = 2 *
      rectangle.diagonal.arbitrarilyDirected.toSeq
        .map({ p => (p → rectangle.corner).length }).sum
  }

  sealed case class Interior(polygon: DiagonalAndCornerRectangle)
      extends Rectangle.InteriorOf[DiagonalAndCornerRectangle]

  sealed case class Exterior(polygon: DiagonalAndCornerRectangle)
      extends Rectangle.ExteriorOf[DiagonalAndCornerRectangle]
}

trait OrthogonalRectangle extends Rectangle {

  def min: CartesianVector
  def max: CartesianVector

  def size: CartesianVector

  override def area: Double = size.x * size.y

  def left   : Double = min.x
  def right  : Double = max.x
  def bottom : Double = min.y
  def top    : Double = max.y

  def bottomLeft: Point = xy ( left, bottom )
  def bottomRight: Point = xy ( right, bottom )
  def topLeft: Point = xy ( left, top )
  def topRight: Point = xy ( right, top )

  override def arbitraryPath: FourPoints =
    Cycle( bottomLeft, bottomRight, topRight, topLeft )

  override def perimeter: OrthogonalRectangle.Perimeter
  override def interior: OrthogonalRectangle.Interior
  override def exterior: OrthogonalRectangle.Exterior
}

object OrthogonalRectangle
    extends RectangleCenterWidthAndHeightFromDiagonal {

  trait PerimeterOf[+A <: OrthogonalRectangle]
      extends Rectangle.PerimeterOf[A] {

    override def length = 2 * (rectangle.size.x + rectangle.size.y)

    def bottom: LineSegment =
      LineSegment(
        xy( rectangle.left, rectangle.bottom ),
        xy( rectangle.right, rectangle.bottom )
      )

    def top: LineSegment = LineSegment(
      xy( rectangle.left, rectangle.top ),
      xy( rectangle.right, rectangle.top )
    )

    def left: LineSegment = LineSegment(
      xy( rectangle.left, rectangle.top ),
      xy( rectangle.left, rectangle.bottom )
    )

    def right: LineSegment = LineSegment(
      xy( rectangle.right, rectangle.top ),
      xy( rectangle.right, rectangle.bottom )
    )
  }

  trait InteriorOf[+A <: OrthogonalRectangle] extends Rectangle.InteriorOf[A]

  trait ExteriorOf[+A <: OrthogonalRectangle] extends Rectangle.ExteriorOf[A]

  type Perimeter = PerimeterOf[OrthogonalRectangle]
  type Interior = InteriorOf[OrthogonalRectangle]
  type Exterior = ExteriorOf[OrthogonalRectangle]
}

sealed case class RectangleCenterWidthAndHeight(center: Point,
    size: CartesianVector) extends OrthogonalRectangle {

  require(size.x >= 0)
  require(size.y >= 0)

  private def dx = size.x / 2
  private def dy = size.y / 2

  override def min: CartesianVector = xy( center.x - dx, center.y - dy )
  override def max: CartesianVector = xy( center.x + dx, center.y + dy )

  override def arbitraryDiagonal: LineSegment = {
    LineSegment( xy(min.x, min.y), xy(max.x, max.y) )
  }

  override def arbitraryDiagonalAndCornerRectangle:
      DiagonalAndCornerRectangle = {
    DiagonalAndCornerRectangle(
      diagonal = LineSegment( xy(min.x, min.y), xy(max.x, max.y) ),
      corner = xy(min.x, max.y)
    )
  }

  override def rotate(angle: AnyRadians): RotatedOrthogonalRectangle =
    RotatedOrthogonalRectangle(this, angle)

  override def rotate(angle: AnyRadians, pivot: Point): Rectangle =
    arbitraryDiagonalAndCornerRectangle.rotate(angle, pivot)

  override def pad(padding: Double): RectangleCenterWidthAndHeight =
    copy(size = size + padding * xy(2, 2))


  override def withArea(newArea: Double): RectangleCenterWidthAndHeight =
    copy(size = {
      val newSizeX = sqrt( size.x * newArea / size.y )
      xy(newSizeX, newArea / newSizeX)
    })

  override def perimeter = RectangleCenterWidthAndHeight.Perimeter(this)
  override def interior = RectangleCenterWidthAndHeight.Interior(this)
  override def exterior = RectangleCenterWidthAndHeight.Exterior(this)
}

trait RectangleCenterWidthAndHeightFromDiagonal {

  def apply(diagonal: LineSegment): RectangleCenterWidthAndHeight =
    RectangleCenterWidthAndHeight(
      center = diagonal.midpoint,
      size = {
        val (a, b) = diagonal.arbitrarilyDirected.toTuple
        xy( math.abs(a.x - b.x), math.abs(a.y - b.y) )
      }
    )
}

object RectangleCenterWidthAndHeight
    extends RectangleCenterWidthAndHeightFromDiagonal {

  sealed case class Perimeter(polygon: RectangleCenterWidthAndHeight)
      extends OrthogonalRectangle.PerimeterOf[RectangleCenterWidthAndHeight]

  sealed case class Interior(polygon: RectangleCenterWidthAndHeight)
      extends OrthogonalRectangle.InteriorOf[RectangleCenterWidthAndHeight]

  sealed case class Exterior(polygon: RectangleCenterWidthAndHeight)
      extends OrthogonalRectangle.ExteriorOf[RectangleCenterWidthAndHeight]
}

sealed case class RotatedOrthogonalRectangle(
    orthogonal: RectangleCenterWidthAndHeight, angle: SemicircleRadians)
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

  override def arbitraryPath = ???

  override def area: Double = orthogonal.area

  override def withArea(newArea: Double): RotatedOrthogonalRectangle =
    copy( orthogonal = orthogonal withArea newArea )

  override val perimeter = RotatedOrthogonalRectangle.Perimeter(this)
  override val interior = RotatedOrthogonalRectangle.Interior(this)
  override val exterior = RotatedOrthogonalRectangle.Exterior(this)
}

object RotatedOrthogonalRectangle {

  sealed case class Perimeter(polygon: RotatedOrthogonalRectangle)
      extends Rectangle.PerimeterOf[RotatedOrthogonalRectangle] {

    override def length = rectangle.orthogonal.perimeter.length
  }

  sealed case class Interior(polygon: RotatedOrthogonalRectangle)
      extends Rectangle.InteriorOf[RotatedOrthogonalRectangle]

  sealed case class Exterior(polygon: RotatedOrthogonalRectangle)
      extends Rectangle.ExteriorOf[RotatedOrthogonalRectangle]
}

trait Rhombus extends Quadrilateral {

  def center: Point

  def edgeLength: Double
}

trait DefinedByRhombus[+A <: Rhombus] extends DefinedByQuadrilateral[A] {

  def rhombus: A = polygon

  override def polygon: A
}

object Rhombus {

  trait PerimeterOf[+A <: Rhombus] extends Quadrilateral.PerimeterOf[A]
      with DefinedByRhombus[A] {

    override def toString: String = s"Rhombus.Perimeter($rhombus)"
  }

  trait InteriorOf[+A <: Rhombus] extends Quadrilateral.InteriorOf[A]
      with DefinedByRhombus[A] {

    override def toString: String = s"Rhombus.Interior($rhombus)"
  }

  trait ExteriorOf[+A <: Rhombus] extends Quadrilateral.ExteriorOf[A]
      with DefinedByRhombus[A] {

    override def toString: String = s"Rhombus.Exterior($rhombus)"
  }

  type Perimeter = PerimeterOf[Rhombus]
  type Interior = InteriorOf[Rhombus]
  type Exterior = ExteriorOf[Rhombus]
}

trait Square extends Rectangle with Rhombus

trait DefinedBySquare[+A <: Square] extends DefinedByRectangle[A]
    with DefinedByRhombus[A] {

  def square: A = polygon

  override def polygon: A
}

object Square {

  trait PerimeterOf[+A <: Square] extends DefinedBySquare[A]
      with Rectangle.PerimeterOf[A] with Rhombus.PerimeterOf[A] {

    override def toString: String = s"Square.Perimeter($square)"
  }

  trait InteriorOf[+A <: Square] extends DefinedBySquare[A]
      with Rectangle.InteriorOf[A] with Rhombus.InteriorOf[A] {

    override def toString: String = s"Square.Interior($square)"
  }

  trait ExteriorOf[+A <: Square] extends DefinedBySquare[A]
      with Rectangle.ExteriorOf[A] with Rhombus.ExteriorOf[A] {

    override def toString: String = s"Square.Exterior($square)"
  }

  type Perimeter = PerimeterOf[Square]
  type Interior = InteriorOf[Square]
  type Exterior = ExteriorOf[Square]
}

sealed case class OrthogonalSquare(center: Point, edgeLength: Double)
    extends OrthogonalRectangle with Square {

  require(edgeLength >= 0)

  private lazy val d = edgeLength / 2

  override def min: CartesianVector = xy(center.x - d, center.y - d)
  override def max: CartesianVector = xy(center.x + d, center.y + d)
  override def size: CartesianVector = xy(edgeLength, edgeLength)

  override def pad(padding: Double): OrthogonalSquare =
    copy(edgeLength = edgeLength + 2 * padding)

  override def rotate(angle: AnyRadians): RotatedOrthogonalSquare =
    RotatedOrthogonalSquare(this, angle)

  override def rotate(angle: AnyRadians, pivot: Point): Square = ???

  override def arbitraryDiagonal: LineSegment =
    LineSegment( xy(min.x, min.y), xy(max.x, max.y) )

  override def arbitraryDiagonalAndCornerRectangle:
      DiagonalAndCornerRectangle =
    DiagonalAndCornerRectangle(
      diagonal = LineSegment( xy(min.x, min.y), xy(max.x, max.y) ),
      corner = xy(min.x, max.y)
    )

  override def area: Double = edgeLength.square

  override def withArea(newArea: Double): OrthogonalSquare =
    copy( edgeLength = sqrt(newArea) )

  override val perimeter = OrthogonalSquare.Perimeter(this)
  override val interior = OrthogonalSquare.Interior(this)
  override val exterior = OrthogonalSquare.Exterior(this)
}

object OrthogonalSquare {

  sealed case class Perimeter(polygon: OrthogonalSquare)
      extends Square.PerimeterOf[OrthogonalSquare]
      with OrthogonalRectangle.PerimeterOf[OrthogonalSquare]

  sealed case class Interior(polygon: OrthogonalSquare)
      extends Square.InteriorOf[OrthogonalSquare]
      with OrthogonalRectangle.InteriorOf[OrthogonalSquare]

  sealed case class Exterior(polygon: OrthogonalSquare)
      extends Square.ExteriorOf[OrthogonalSquare]
      with OrthogonalRectangle.ExteriorOf[OrthogonalSquare]
}

sealed case class RotatedOrthogonalSquare(orthogonal: OrthogonalSquare,
    angle: QuarterCircleRadians) extends Square {

  override def center = orthogonal.center

  override def edgeLength = orthogonal.edgeLength

  override def arbitraryDiagonal = ???

  override def arbitraryDiagonalAndCornerRectangle = ???

  override def rotate(angle: AnyRadians) = ???

  override def rotate(angle: AnyRadians, pivot: Point) = ???

  override def pad(padding: Double): RotatedOrthogonalSquare =
    copy(orthogonal = orthogonal.pad(padding))

  override def area = orthogonal.area

  override def withArea(newArea: Double) =
    copy( orthogonal = orthogonal withArea newArea )

  override def arbitraryPath = ???

  override val perimeter = RotatedOrthogonalSquare.Perimeter(this)
  override val interior = RotatedOrthogonalSquare.Interior(this)
  override val exterior = RotatedOrthogonalSquare.Exterior(this)
}

object RotatedOrthogonalSquare {

  sealed case class Perimeter(polygon: RotatedOrthogonalSquare)
      extends Square.PerimeterOf[RotatedOrthogonalSquare] {

    override def length: Double = square.orthogonal.perimeter.length
  }

  sealed case class Interior(polygon: RotatedOrthogonalSquare)
      extends Square.InteriorOf[RotatedOrthogonalSquare]

  sealed case class Exterior(polygon: RotatedOrthogonalSquare)
      extends Square.ExteriorOf[RotatedOrthogonalSquare]
}

sealed case class FourPoints(a: Point, b: Point, c: Point, d: Point)
    extends Cycle with Quadrilateral {

  override def arbitraryPath = this

  override def vertices = Seq(a, b, c, d)

  override def area = ???

  def perimeter = FourPoints.Perimeter(this)
  def interior = FourPoints.Interior(this)
  def exterior = FourPoints.Exterior(this)
}

object FourPoints {

  sealed case class Perimeter(polygon: FourPoints)
      extends Quadrilateral.PerimeterOf[FourPoints] {

    override def length: Double =
      (polygon.a → polygon.b).length +
      (polygon.b → polygon.c).length +
      (polygon.c → polygon.d).length +
      (polygon.d → polygon.a).length
  }

  sealed case class Interior(polygon: FourPoints)
    extends Quadrilateral.InteriorOf[FourPoints]

  sealed case class Exterior(polygon: FourPoints)
    extends Quadrilateral.ExteriorOf[FourPoints]
}
