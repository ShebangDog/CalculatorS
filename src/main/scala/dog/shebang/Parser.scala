package dog.shebang

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {
  def expr: Parser[Int] = (term ~ rep(secondary_operator ~ term)) ^^ {
    case fact ~ list => list.foldLeft(fact) { (res, elem) =>
      elem match {
        case "+" ~ fact => res + fact
        case "-" ~ fact => res - fact
      }
    }
  }

  def term: Parser[Int] = (factor ~ rep(primary_operator ~ factor)) ^^ {
    case num ~ list => list.foldLeft(num) { (res, elem) =>
      elem match {
        case "*" ~ num => res * num
        case "/" ~ num => res / num
      }
    }
  }

  def factor: Parser[Int] = wholeNumber ^^ {
    _.toInt
  } | "(" ~ expr ~ ")" ^^ {
    case "(" ~ num ~ ")" => num
    case _ => ???
  }

  def primary_operator: Parser[String] = "*" | "/"

  def secondary_operator: Parser[String] = "+" | "-"

}
