package space.geometry.dimension2

object ArrowOps {

  trait ArrowOperator[B, C] {

    def →(b: B): C

    def ->(b: B): C = →(b)
  }

  trait TwoPointsArrowOperator[B] extends ArrowOperator[B, TwoPoints] {

    def --(b: B): LineSegment = →(b).toLineSegment
  }
}

trait ArrowOps {

  import ArrowOps._

  def distance[A <% TwoPointsArrowOperator[B], B](a: A, b: B): Double =
    ((a: TwoPointsArrowOperator[B]) -- b).length

  implicit class `Point → Point`(left: Point)
      extends TwoPointsArrowOperator[Point] {

    override def →(right: Point): TwoPoints = TwoPoints(left, right)
  }

  implicit class `Point → LineLike`(point: Point)
      extends TwoPointsArrowOperator[LineLike] {

    override def →(line: LineLike): TwoPoints =
      point → (line pointClosestTo point)
  }

  implicit class `LineLike → Point`(line: LineLike)
      extends TwoPointsArrowOperator[Point] {

    override def →(point: Point): TwoPoints =
      (line pointClosestTo point) → point
  }
}
