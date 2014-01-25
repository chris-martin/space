package space.geometry.dimension2

trait ArrowOps {

  trait ArrowOperator[Right, Result] {

    def →(b: Right): Result

    def ->(b: Right): Result = →(b)
  }

  implicit class ArrowVectorToVector(left: Vector)
      extends ArrowOperator[Vector, TwoPoints] {

    override def →(right: Vector): TwoPoints = TwoPoints(left, right)
  }

  implicit class ArrowVectorToLineLike(vector: Vector)
      extends ArrowOperator[LineLike, TwoPoints] {

    override def →(line: LineLike): TwoPoints =
      vector → (line pointClosestTo vector)
  }

  implicit class ArrowLineLikeToVector(line: LineLike)
      extends ArrowOperator[Vector, TwoPoints] {

    override def →(vector: Vector): TwoPoints =
      (line pointClosestTo vector) → vector
  }
}
