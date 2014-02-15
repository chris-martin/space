package space.geometry
package dimension2

import math.abs

trait Quadrilateral extends Polygon {

  override def arbitraryPath: FourPoints
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

trait OrthogonalRectangle extends Rectangle {

  def min: CartesianVector
  def max: CartesianVector

  def size: CartesianVector

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

  override def perimeterLength: Double = 2 * (size.x + size.y)
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
      xy( newSizeX, newArea / newSizeX )
    })

}

trait RectangleCenterWidthAndHeightFromDiagonal {

  def apply(diagonal: LineSegment): RectangleCenterWidthAndHeight =
    RectangleCenterWidthAndHeight(
      center = diagonal.midpoint,
      size = diagonal.arbitrarilyDirected.difference.toCartesian map abs
    )
}

object RectangleCenterWidthAndHeight
    extends RectangleCenterWidthAndHeightFromDiagonal

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

trait Square extends Rectangle with Rhombus

sealed case class OrthogonalSquare(center: Point, edgeLength: Double)
    extends OrthogonalRectangle with Square {

  require(edgeLength >= 0)

  private lazy val d = edgeLength / 2

  override def min: CartesianVector = xy( center.x - d, center.y - d )
  override def max: CartesianVector = xy( center.x + d, center.y + d )
  override def size: CartesianVector = xy( edgeLength, edgeLength )

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
