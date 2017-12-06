package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  /**
    * @param namedExpressions e.g. Map("a", Signal(Plus(Literal(3.5), Ref("a"))))
    * @return "a", Signal()
    */
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map {
      case (valName, signalExpr) => valName -> Signal(eval(signalExpr(), namedExpressions))
    }
  }

  /**
    * @param expr Signal(Plus(Literal(3.5), Ref("a")))
    * @param references Map("a", Signal(Plus(Literal(3.5), Ref("a"))))
    * @return
    */
  // getReferenceExpr("a", references), references.filterKeys(_ != n)
  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v) => v
    case Ref(n) => eval(getReferenceExpr(n, references), references.filterKeys(_ != n))
    case Plus(a, b) => eval(a, references) + eval(b, references)
    case Minus(a, b) => eval(a, references) - eval(b, references)
    case Times(a, b) => eval(a, references) * eval(b, references)
    case Divide(a, b) => eval(a, references) / eval(b, references)
  }

  /**
    * Get the Expr for a referenced variables.
    *  If the variable is not known, returns a literal NaN.
    *
    * @param name e.g. "a"
    * @param references e.g. Map("a", Signal(Plus(Literal(3.5), Ref("a"))))
    * @return exist Expr e.g. Signal(Plus(Literal(3.5), Ref("a"))) not exist Double.NaN
    */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal => exprSignal() }
  }
}
