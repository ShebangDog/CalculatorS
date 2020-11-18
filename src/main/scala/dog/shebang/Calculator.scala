package dog.shebang

import dog.shebang.AST.{Arithmetic, Expression, Statement}
import dog.shebang.Parser.program

object Calculator {

  def emit(statement: Statement): Unit = statement match {
    case AST.Declare(ident, value) => "declare " + ident + " as " + eval(value)
    case AST.Print(value) => print(eval(value))
    case AST.Println(value) => println(eval(value))
    case AST.Line(value) => eval(value)
    case AST.None => "None"
  }

  def eval(expr: Expression): Double = expr match {
    case AST.Number(value) => value
    case arithmetic: Arithmetic => arithmetic match {
      case AST.Addition(left, right) => eval(left) + eval(right)
      case AST.Subtraction(left, right) => eval(left) - eval(right)
      case AST.Multiplication(left, right) => eval(left) * eval(right)
      case AST.Division(left, right) => eval(left) / eval(right)
    }
  }

  def main(args: Array[String]): Unit = {
    Parser.parseAll(program,
      """
        |
        |
        | val ident = (22 + 33) * 2
        | 1 + 1 + ident
        |
        | print(ident)
        |
        | """.stripMargin).get
      .foreach(emit)
  }

}
