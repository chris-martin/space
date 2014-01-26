package space.geometry
package dimension2

trait Rhombus {

  def center: Point

  def edgeLength: Double
}

object Rhombus {

  trait Has {

    def rhombus: Rhombus
  }

  trait Perimeter extends Has {

    override def toString: String = s"Rhombus.Perimeter($rhombus)"
  }

  trait Interior extends Has {

    override def toString: String = s"Rhombus.Interior($rhombus)"
  }

  trait Exterior extends Has {

    override def toString: String = s"Rhombus.Exterior($rhombus)"
  }
}
