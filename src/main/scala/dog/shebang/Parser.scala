package dog.shebang

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {
  def expr: Parser[AST] = (term ~ rep(secondary_operator ~ term)) ^^ {
    case value ~ Nil => value
    case t ~ rest => rest.foldLeft(t) {
      case (l, op ~ r) => Node(op, Seq(l, r))
    }
  }

  def term: Parser[AST] = factor ~ rep(primary_operator ~ factor) ^^ {
    case fact ~ Nil => fact
    case fact ~ rest => rest.foldLeft(fact) {
      case (l, op ~ r) => Node(op, Seq(l, r))
    }
  }

  def factor: Parser[AST] = wholeNumber ^^ (num => Number(num.toDouble)) | "(" ~ expr ~ ")" ^^ {
    case "(" ~ num ~ ")" => num
  }

  def primary_operator: Parser[String] = "*" | "/"

  def secondary_operator: Parser[String] = "+" | "-"

}
