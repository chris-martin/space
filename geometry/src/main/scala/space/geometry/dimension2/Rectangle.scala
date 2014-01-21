package space.geometry
package dimension2

trait Rectangle { self =>

  def center: Vector

  def arbitraryDiagonal: LineSegment

  def arbitraryDiagonalAndCorner: DiagonalAndCornerRectangle

  /** Rotation about the center.
   */
  def rotate(angle: AnyRadians): Rectangle

  /** Rotation about a pivot.
   */
  def rotate(angle: AnyRadians, pivot: Vector): Rectangle

  def perimeter: Rectangle.Perimeter

  def interior: Rectangle.Interior

  def exterior: Rectangle.Exterior

  protected trait Self extends Rectangle.Has {
    override def rectangle: Rectangle = self
  }
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

    override def toString: String = s"Rectangle.Interior($rectangle)"
  }

  trait Exterior extends Has {

    override def toString: String = s"Rectangle.Exterior($rectangle)"
  }
}

sealed case class DiagonalAndCornerRectangle(
diagonal: LineSegment, corner: Vector) extends Rectangle {

  override def center: Vector = diagonal.midpoint

  override def arbitraryDiagonal: LineSegment = diagonal

  override def arbitraryDiagonalAndCorner:
  DiagonalAndCornerRectangle = this

  override def rotate(angle: AnyRadians):
  DiagonalAndCornerRectangle = DiagonalAndCornerRectangle(
  diagonal = diagonal.rotate(angle),
  corner = corner.rotate(angle, center))

  override def rotate(angle: AnyRadians, pivot: Vector):
  DiagonalAndCornerRectangle = DiagonalAndCornerRectangle(
  diagonal = diagonal.rotate(angle, pivot),
  corner = corner.rotate(angle, pivot))

  def otherCorner: Vector = corner rotate (Radians.halfCircle, center)

  def flipCorner: DiagonalAndCornerRectangle =
  DiagonalAndCornerRectangle(diagonal, otherCorner)

  object perimeter extends Rectangle.Perimeter with Self {

    override def length = 2 *
    diagonal.arbitrarilyDirected.toSeq.map(p => (p → corner).length).sum
  }

  object interior extends Rectangle.Interior with Self {

    override def area: Double = {
      val d = diagonal.arbitrarilyDirected
      val triangle = Triangle(d.source, d.destination, corner)
      2 * triangle.interior.area
    }
  }

  object exterior extends Rectangle.Exterior with Self
}

sealed case class OrthogonalRectangle(center: Vector,
sizeX: Double, sizeY: Double) extends Rectangle {

  require(sizeX >= 0)
  require(sizeY >= 0)

  private lazy val dx = sizeX / 2
  private lazy val dy = sizeY / 2

  override def arbitraryDiagonal: LineSegment = LineSegment(
  xy(center.x - dx, center.y - dy),
  xy(center.x + dy, center.y + dy))

  override def arbitraryDiagonalAndCorner:
  DiagonalAndCornerRectangle = DiagonalAndCornerRectangle(
    diagonal = LineSegment(
      xy(center.x - dx, center.y - dy),
      xy(center.x + dy, center.y + dy)),
    corner = xy(center.x - dx, center.y + dy)
  )

  override def rotate(angle: AnyRadians):
  RotatedOrthogonalRectangle = RotatedOrthogonalRectangle(this, angle)

  override def rotate(angle: AnyRadians, pivot: Vector):
  Rectangle = arbitraryDiagonalAndCorner.rotate(angle, pivot)

  object perimeter extends Rectangle.Perimeter with Self {

    override def length = 2 * (sizeX + sizeY)

    def bottom: LineSegment = LineSegment(
    xy(center.x - dx, center.y - dy),
    xy(center.x + dx, center.y - dy))

    def top: LineSegment = LineSegment(
    xy(center.x - dx, center.y + dy),
    xy(center.x + dx, center.y + dy))

    def left: LineSegment = LineSegment(
    xy(center.x - dx, center.y + dy),
    xy(center.x - dx, center.y - dy))

    def right: LineSegment = LineSegment(
    xy(center.x + dx, center.y + dy),
    xy(center.x + dx, center.y - dy))
  }

  object interior extends Rectangle.Interior with Self {

    override def area: Double = sizeX * sizeY
  }

  object exterior extends Rectangle.Exterior with Self
}

object OrthogonalRectangle {

  def apply(c1: Vector, c2: Vector): OrthogonalRectangle =
  OrthogonalRectangle(center = (c1->c2).midpoint,
  sizeX = math.abs(c1.x - c2.x), sizeY = math.abs(c1.y - c2.y))
}

sealed case class RotatedOrthogonalRectangle(
orthogonal: OrthogonalRectangle, angle: SemicircleRadians)
extends Rectangle {

  override def center: Vector = orthogonal.center

  override def arbitraryDiagonal: LineSegment =
  orthogonal.arbitraryDiagonal.rotate(angle.toAnyRadiansArbitrarily)

  override def arbitraryDiagonalAndCorner: DiagonalAndCornerRectangle =
  orthogonal.arbitraryDiagonalAndCorner.rotate(angle.toAnyRadiansArbitrarily)

  override def rotate(angle: AnyRadians):
  RotatedOrthogonalRectangle = RotatedOrthogonalRectangle(
  orthogonal, this.angle + angle)

  override def rotate(angle: AnyRadians, pivot: Vector):
  Rectangle = arbitraryDiagonalAndCorner.rotate(angle, pivot)

  object perimeter extends Rectangle.Perimeter with Self {

    override def length = orthogonal.perimeter.length
  }

  object interior extends Rectangle.Interior with Self {

    override def area: Double = orthogonal.interior.area
  }

  object exterior extends Rectangle.Exterior with Self
}
