package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for(ex <- namedExpressions) yield (ex._1, new Signal(eval(ex._2(), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def calculate(e: Expr, usedFields: List[Expr]): Double = {
        e match {
          case Literal(v) => v
          case Ref(n) => {
            val newExpr = getReferenceExpr(n, references)
            if(usedFields contains newExpr) Double.NaN
            else calculate(newExpr, newExpr :: usedFields)
          }
          case Plus(a, b) => calculate(a, a :: b :: usedFields) + calculate(b, a :: b :: usedFields)
          case Minus(a, b) => calculate(a, a :: b :: usedFields) - calculate(b, a :: b :: usedFields)
          case Times(a, b) => calculate(a, a :: b :: usedFields) * calculate(b, a :: b :: usedFields)
          case Divide(a, b) => calculate(a, a:: b :: usedFields) / calculate(b, a :: b :: usedFields)
        }

      def cal(a: Expr, b: Expr, fields: List[Expr], f:(Double, Double) => Double): Double = {
        if((fields contains a) || (fields contains b)) Double.NaN
        else f(a, b)
      }
    }
    calculate(expr, List())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal => exprSignal()
    }
  }
}
