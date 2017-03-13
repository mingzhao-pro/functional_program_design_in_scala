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
    for(ex <- namedExpressions) yield (ex._1, new Signal(eval(new Ref(ex._1), namedExpressions)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def calculate(e: Expr, lastE: Expr): Double = {
      e match {
        case Literal(v) => v
        case Ref(n) => {
          val newExpr = getReferenceExpr(n, references)
          if (newExpr == Literal(Double.NaN) || lastE.equals(newExpr)) Double.NaN
          else calculate(newExpr, e)
        }
        case Plus(a, b)  => {
//          println("e " + e + " a " + a + " lastE " + lastE)
          if(e.toString.contains(a.toString)
            || e.toString.contains(b.toString)
            || lastE.toString.equals(a.toString)
            || lastE.toString.equals(b.toString))
            Double.NaN
          else calculate(a, a) + calculate(b, b)
        }
        case Minus(a, b) => calculate(a, a) - calculate(b, b)
        case Times(a, b) => calculate(a, a) * calculate(b, b)
        case Divide(a, b) => calculate(a, a) / calculate(b, b)
      }
    }
    calculate(expr, expr)
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
