package dog.shebang.front

object Evaluator {
  def eval(expr: AST.Expression): AST.Number = expr match {
    case number: AST.Number => number
    case arithmetic: AST.Arithmetic => arithmetic match {
      case AST.Addition(left, right) => eval(left) + eval(right)
      case AST.Subtraction(left, right) => eval(left) - eval(right)
      case AST.Multiplication(left, right) => eval(left) * eval(right)
      case AST.Division(left, right) => eval(left) / eval(right)
    }
  }
}
