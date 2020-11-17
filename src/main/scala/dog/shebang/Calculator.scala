package dog.shebang

import dog.shebang.Parser.expr

object Calculator {

  def eval(ast: AST): Double = ast match {
    case Number(value) => value
    case Node("+", Seq(l, r)) => eval(l) + eval(r)
    case Node("-", Seq(l, r)) => eval(l) - eval(r)
    case Node("*", Seq(l, r)) => eval(l) * eval(r)
    case Node("/", Seq(l, r)) => eval(l) / eval(r)
  }

  def main(args: Array[String]): Unit = {
    println(eval(Parser.parseAll(expr, """(22 + 33) * 2""").get))
  }

}
