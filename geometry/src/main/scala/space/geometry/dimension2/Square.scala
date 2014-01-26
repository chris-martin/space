package space.geometry
package dimension2

trait Square extends Rectangle with Rhombus

object Square {

  trait Has extends Rectangle.Has with Rhombus.Has {

    def square: Square

    override def rectangle: Rectangle = square

    override def rhombus: Rhombus = square
  }

  trait Perimeter extends Has with Rectangle.Perimeter with Rhombus.Perimeter {

    override def toString: String = s"Square.Perimeter($square)"
  }

  trait Interior extends Has with Rectangle.Interior with Rhombus.Interior {

    override def toString: String = s"Square.Interior($square)"
  }

  trait Exterior extends Has with Rectangle.Exterior with Rhombus.Exterior {

    override def toString: String = s"Square.Exterior($square)"
  }
}

sealed case class OrthogonalSquare(center: Point, edgeLength: Double)
    extends Square {

  require(edgeLength >= 0)
  
  private lazy val d = edgeLength / 2

  object bound extends OrthogonalBoundingBox {
    override def min: CartesianVector = xy(center.x - d, center.y - d)
    override def max: CartesianVector = xy(center.x + d, center.y + d)
  }

  override def pad(padding: Double): OrthogonalSquare =
    copy(edgeLength = edgeLength + 2 * padding)

  override def rotate(angle: AnyRadians): RotatedOrthogonalSquare = 
    RotatedOrthogonalSquare(this, angle)

  override def rotate(angle: AnyRadians, pivot: Point): Square = ???

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

  override val perimeter = OrthogonalSquare.Perimeter(this)

  override val interior = OrthogonalSquare.Interior(this)

  override val exterior = OrthogonalSquare.Exterior(this)
}

object OrthogonalSquare {

  sealed case class Perimeter(square: OrthogonalSquare)
      extends Square.Perimeter {

    import square._

    override def length = 4 * edgeLength

    def bottom: LineSegment = bound.edge.bottom
    def top: LineSegment = bound.edge.top
    def left: LineSegment = bound.edge.left
    def right: LineSegment = bound.edge.right
  }

  sealed case class Interior(square: OrthogonalSquare)
      extends Square.Interior {

    import square._

    override def area: Double = edgeLength.square

    override def withArea(newArea: Double): OrthogonalSquare.Interior =
      square.copy(edgeLength = sqrt(newArea)).interior
  }

  sealed case class Exterior(square: OrthogonalSquare) extends Square.Exterior
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

  override val perimeter = RotatedOrthogonalSquare.Perimeter(this)

  override val interior = RotatedOrthogonalSquare.Interior(this)

  override val exterior = RotatedOrthogonalSquare.Exterior(this)
}

object RotatedOrthogonalSquare {

  sealed case class Perimeter(square: RotatedOrthogonalSquare)
      extends Square.Perimeter {

    import square._

    override def length: Double = orthogonal.perimeter.length
  }

  sealed case class Interior(square: RotatedOrthogonalSquare)
      extends Square.Interior {

    import square._

    override def area: Double = orthogonal.interior.area

    override def withArea(newArea: Double): RotatedOrthogonalSquare.Interior =
      square.copy(
        orthogonal = orthogonal.interior.withArea(newArea).square
      ).interior
  }

  sealed case class Exterior(square: RotatedOrthogonalSquare)
      extends Square.Exterior
}
