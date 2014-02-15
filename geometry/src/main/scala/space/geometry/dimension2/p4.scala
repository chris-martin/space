package space.geometry
package dimension2

import math.{abs, tan, sin, cos}

trait Quadrilateral extends Polygon {

  override def arbitraryPath: FourPoints
}

trait OrthogonalQuadrilateral {

  /** A vector in the first quadrant (having nonnegative coordinates)
    * where the `x` value is the `width` of the quad and the `y` value
    * is the `height` of the quad.
    */
  def size: CartesianVector
  
  def width: Double
  def height: Double
}

object OrthogonalQuadrilateral {
  
  trait DefinesSize extends OrthogonalQuadrilateral {
    override def width = size.x
    override def height = size.y
  }
  
  trait DefinesWidthAndHeight extends OrthogonalQuadrilateral {
    override def size = xy(width, height)
  }
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
      ( distance(a, corner), distance(b, corner) )
    }

    val newDiagonalLength = sqrt(newArea * (a/b + b/a))

    DiagonalAndCornerRectangle(
      diagonal = diagonal withLength newDiagonalLength,
      corner = (flipCorner.diagonal withLength newDiagonalLength)
        .arbitrarilyDirected.source
    )
  }

  override def perimeterLength: Double =
    2 * diagonal.arbitrarilyDirected.toSeq.map(distance(corner, _)).sum
}

trait OrthogonalRectangle extends Rectangle with OrthogonalQuadrilateral {

  def min: CartesianVector
  def max: CartesianVector

  override def area: Double = size.x * size.y

  def left   : Double = min.x
  def right  : Double = max.x
  def bottom : Double = min.y
  def top    : Double = max.y

  def bottomLeft:  Point = xy ( left,  bottom )
  def bottomRight: Point = xy ( right, bottom )
  def topLeft:     Point = xy ( left,  top    )
  def topRight:    Point = xy ( right, top    )

  def bottomEdge: LineSegment = xy( left,  bottom ) -- xy( right, bottom )
  def topEdge:    LineSegment = xy( left,  top    ) -- xy( right, top    )
  def leftEdge:   LineSegment = xy( left,  top    ) -- xy( left,  bottom )
  def rightEdge:  LineSegment = xy( right, top    ) -- xy( right, bottom )

  override def arbitraryPath: FourPoints =
    Cycle( bottomLeft, bottomRight, topRight, topLeft )

  override def perimeterLength: Double = 2 * (width + height)
}

object OrthogonalRectangle
    extends ConstructsRectangleCenterWidthAndHeightFromDiagonal

sealed case class RectangleCenterWidthAndHeight(override val center: Point,
    override val size: CartesianVector) extends OrthogonalRectangle 
    with OrthogonalQuadrilateral.DefinesSize {

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
      xy( newSizeX, newArea / newSizeX )
    })

}

trait ConstructsRectangleCenterWidthAndHeightFromDiagonal {

  def apply(diagonal: LineSegment): RectangleCenterWidthAndHeight =
    RectangleCenterWidthAndHeight(
      center = diagonal.midpoint,
      size = diagonal.arbitrarilyDirected.difference.toCartesian map abs
    )
}

object RectangleCenterWidthAndHeight
    extends ConstructsRectangleCenterWidthAndHeightFromDiagonal

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
    arbitraryDiagonalAndCornerRectangle rotate (angle, pivot)

  override def pad(padding: Double): RotatedOrthogonalRectangle =
    copy( orthogonal = orthogonal pad padding )

  override def arbitraryPath = ???

  override def area: Double = orthogonal.area

  override def withArea(newArea: Double): RotatedOrthogonalRectangle =
    copy( orthogonal = orthogonal withArea newArea )

  override def perimeterLength = orthogonal.perimeterLength
}

trait Rhombus extends Quadrilateral {

  def center: Point

  def edgeLength: Double
}

/** {{{
  *        c1
  *       / \           c:  center
  *      /   \          c1: corner1
  *     /     \         c2: corner2
  *    /   c   \c2
  *    \       /
  *     \     /
  *      \   /
  *       \ /
  * }}}
  */
sealed case class CenterAndTwoCornersRhombus(
    center: Point, corner1: Point, corner2: Point) extends Rhombus {

  override def area: Double = 4 * Triangle(center, corner1, corner2).area

  override def edgeLength: Double = distance(corner1, corner2)

  override def arbitraryPath: FourPoints =
    FourPoints(
      corner1,
      corner2,
      corner1 + 2 * (corner1 → center).difference,
      corner2 + 2 * (corner2 → center).difference
    )
}

trait OrthogonalRhombus extends Rhombus with OrthogonalQuadrilateral {

  /** A vector in the first quadrant (having nonnegative coordinates)
    * where the `x` value is the width of the rhombus (the length of
    * `horizontalDiagonal`) and the `y` value is the height of the rhombus
    * (the length of `verticalDiagonal`).
    */
  override def size: CartesianVector
  
  def top:    Point = center + xy(0, height/2)
  def bottom: Point = center - xy(0, height/2)
  def left:   Point = center + xy(width/2, 0)
  def right:  Point = center + xy(width/2, 0)

  override def edgeLength: Double = distance(top, right)

  /** The diagonal that is parallel to the X axis.
    */
  def horizonalDiagonal: LineSegment = left -- right

  /** The diagonal that is parallel to the Y axis.
    */
  def verticalDiagonal: LineSegment = top -- bottom

  /** The angle of the rhombus at corners that lie on its horizontal diagonal.
    */
  def horizontalDiagonalAngle: SemicircleRadians

  /** The angle of the rhombus at corners that lie on its vertical diagonal.
   */
  def verticalDiagonalAngle: SemicircleRadians

  override def area: Double =
    horizonalDiagonal.length * verticalDiagonal.length / 2

  override def arbitraryPath = FourPoints(top, right, bottom, left)
}

object OrthogonalRhombus {

  def apply(center: Point, size: CartesianVector) =
    OrthogonalRhombusBySize( center=center, size=size )
}

sealed case class OrthogonalRhombusBySize(override val center: Point,
    override val size: CartesianVector) extends OrthogonalRhombus
    with OrthogonalQuadrilateral.DefinesSize {

  override def horizontalDiagonalAngle = ???
  override def verticalDiagonalAngle = ???
}

trait OrthogonalRhombusByVerticalDiagonalAngle extends OrthogonalRhombus {

  override def horizontalDiagonalAngle: SemicircleRadians =
    -verticalDiagonalAngle
}

/** {{{
  *                 _        c: center
  *       / \        |       a: verticalDiagonalAngle
  *      /   \       |       h: height
  *     /     \      |                       y axis
  *    /   c   \     | h                       |
  *    \       /     |                         |
  *     \  _  /      |                    -----+----- x axis
  *      \/a\/       |                         |
  *       \ /       _|                         |
  * }}}
  */
sealed case class OrthogonalRhombusByVerticalDiagonalAngleAndHeight(
    override val center: Point,
    override val verticalDiagonalAngle: SemicircleRadians,
    override val height: Double) 
    extends OrthogonalRhombusByVerticalDiagonalAngle
    with OrthogonalQuadrilateral.DefinesWidthAndHeight {

  override lazy val width: Double =
    height * tan(verticalDiagonalAngle.toDouble / 2)
}

sealed case class OrthogonalRhombusByVerticalDiagonalAngleAndEdgeLength(
    override val center: Point,
    override val verticalDiagonalAngle: SemicircleRadians,
    override val edgeLength: Double) 
    extends OrthogonalRhombusByVerticalDiagonalAngle
    with OrthogonalQuadrilateral.DefinesWidthAndHeight {

  override lazy val width: Double =
    2 * edgeLength * sin(verticalDiagonalAngle.toDouble / 2)

  override lazy val height: Double =
    2 * edgeLength * cos(verticalDiagonalAngle.toDouble / 2)
}

trait Square extends Rectangle with Rhombus

sealed case class OrthogonalSquare(override val center: Point,
    override val edgeLength: Double) extends OrthogonalRectangle with Square
    with OrthogonalQuadrilateral.DefinesWidthAndHeight {

  require(edgeLength >= 0)

  private lazy val d = edgeLength / 2

  override def min: CartesianVector = xy( center.x - d, center.y - d )
  override def max: CartesianVector = xy( center.x + d, center.y + d )

  override def width = edgeLength
  override def height = edgeLength

  override def pad(padding: Double): OrthogonalSquare =
    copy( edgeLength = edgeLength + 2 * padding )

  override def rotate(angle: AnyRadians): RotatedOrthogonalSquare =
    RotatedOrthogonalSquare(this, angle)

  override def rotate(angle: AnyRadians, pivot: Point): Square = ???

  override def arbitraryDiagonal: LineSegment =
    LineSegment( xy(min.x, min.y), xy(max.x, max.y) )

  override def arbitraryDiagonalAndCornerRectangle:
      DiagonalAndCornerRectangle =
    DiagonalAndCornerRectangle(
      diagonal = LineSegment( xy(min.x, min.y), xy(max.x, max.y) ),
      corner = xy( min.x, max.y )
    )

  override def area: Double = edgeLength.square

  override def withArea(newArea: Double): OrthogonalSquare =
    copy( edgeLength = sqrt(newArea) )
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
    copy( orthogonal = orthogonal pad padding )

  override def area = orthogonal.area

  override def withArea(newArea: Double) =
    copy( orthogonal = orthogonal withArea newArea )

  override def arbitraryPath = ???

  override def perimeterLength: Double = orthogonal.perimeterLength
}

sealed case class FourPoints(a: Point, b: Point, c: Point, d: Point)
    extends Cycle with Quadrilateral {

  override def arbitraryPath = this

  override def vertices = Seq(a, b, c, d)

  override def area = ???
}
