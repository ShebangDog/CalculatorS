package dog.shebang.front

import dog.shebang.front.Parser.program

object Calculator {

  def emit(statement: AST.Statement): Unit = statement match {
    case AST.Declare(ident, value, t) => eval(value)
    case AST.Print(value) => print(eval(value).makeString())
    case AST.Println(value) => println(eval(value).makeString())
    case AST.Line(value) => eval(value)
    case AST.None => "None"
  }

  def eval(expr: AST.Expression): AST.Number = expr match {
    case number: AST.Number => number
    case arithmetic: AST.Arithmetic => arithmetic match {
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
        | val ident: Int = (22 + 33) * 2
        | 1 + 1 + ident
        |
        | print(ident)
        |
        | """.stripMargin).get
      .foreach(emit)
  }

}