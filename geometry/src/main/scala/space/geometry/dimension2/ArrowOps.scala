package space.geometry.dimension2

trait ArrowOps {

  trait ArrowOperator[Right, Result] {

    def →(b: Right): Result

    def ->(b: Right): Result = →(b)
  }

  implicit class `Point → Point`(left: Point)
      extends ArrowOperator[Point, TwoPoints] {

    override def →(right: Point): TwoPoints = TwoPoints(left, right)
  }

  implicit class `Point → LineLike`(point: Point)
      extends ArrowOperator[LineLike, TwoPoints] {

    override def →(line: LineLike): TwoPoints =
      point → (line pointClosestTo point)
  }

  implicit class `LineLike → Point`(line: LineLike)
      extends ArrowOperator[Point, TwoPoints] {

    override def →(point: Point): TwoPoints =
      (line pointClosestTo point) → point
  }
}
