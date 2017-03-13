package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    new Signal(b() * b() - 4 * a() * c())


  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
        new Signal(delta() match {
          case d if (d >= 0) => {
            val r = Math.sqrt(delta())
            val solution1 = ((-b() + r) / (2 * a()))
            val solution2 = ((-b() - r) / (2 * a()))
            Set(solution1, solution2)
            }
          case d if(d < 0) => Set()
          }
        )
}
