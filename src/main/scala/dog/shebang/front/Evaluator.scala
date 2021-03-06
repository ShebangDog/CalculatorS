package dog.shebang.front

import dog.shebang.table.SymbolMap

object Evaluator {
  def eval(expr: AST.Expression): AST.Number = expr match {
    case number: AST.Number => number
    case AST.Value(id) => SymbolMap.value(id).get
    case function: AST.Function => eval(function.expression.last)
    case arithmetic: AST.Arithmetic => arithmetic match {
      case AST.Addition(left, right) => eval(left) + eval(right)
      case AST.Subtraction(left, right) => eval(left) - eval(right)
      case AST.Multiplication(left, right) => eval(left) * eval(right)
      case AST.Division(left, right) => eval(left) / eval(right)
    }
  }

  def evalAsType(expr: AST.Expression): AST.Type = eval(expr) match {
    case AST.IntNumber(_) => AST.Int
    case AST.DoubleNumber(_) => AST.Double
  }
}
