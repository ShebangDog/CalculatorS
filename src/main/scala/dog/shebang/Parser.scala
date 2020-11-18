package dog.shebang

import dog.shebang.AST.{Addition, Declare, Division, DoubleNumber, Expression, Int, IntNumber, Line, Multiplication, Number, Statement, Subtraction}

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {
  def program: Parser[List[Statement]] = rep(statement ~ rep("""\n""")) ^^ { program =>
    program.foldRight(List(AST.None): List[Statement]) { case (line ~ _, res) => line :: res }
  }

  def statement: Parser[Statement] = "val" ~ ident ~ ":" ~ type_info ~ "=" ~ expr ^^ {
    case _ ~ id ~ _ ~ t ~ _ ~ exp =>
      SymbolMap.declareIdent(id, exp)
      Declare(id, exp, t)
  } |
    "print" ~ "(" ~ expr ~ ")" ^^ { case _ ~ _ ~ exp ~ _ => AST.Print(exp) } |
    "println" ~ "(" ~ expr ~ ")" ^^ { case _ ~ _ ~ exp ~ _ => AST.Println(exp) } |
    expr ^^ Line

  def expr: Parser[Expression] = (term ~ rep(secondary_operator ~ term)) ^^ {
    case value ~ Nil => value
    case t ~ rest => rest.foldLeft(t) {
      case (l, "+" ~ r) => Addition(l, r)
      case (l, "-" ~ r) => Subtraction(l, r)
    }
  }

  def term: Parser[Expression] = factor ~ rep(primary_operator ~ factor) ^^ {
    case fact ~ Nil => fact
    case fact ~ rest => rest.foldLeft(fact) {
      case (l, "*" ~ r) => Multiplication(l, r)
      case (l, "/" ~ r) => Division(l, r)
    }
  }

  def factor: Parser[Expression] = wholeNumber ^^ (num => IntNumber(num.toInt)) |
    floatingPointNumber ^^ (floatNum => DoubleNumber(floatNum.toDouble)) |
    "(" ~ expr ~ ")" ^^ { case "(" ~ num ~ ")" => num } |
    ident ^^ { id =>
      SymbolMap.getIdent(id) match {
        case Some(value) => value
        case _ => ???
      }
    }

  def type_info: Parser[AST.Type] = ident ^^ {
    case AST.Int.typeName => AST.Int
    case AST.Double.typeName => AST.Double
    case _ => ???
  }

  def primary_operator: Parser[String] = "*" | "/"

  def secondary_operator: Parser[String] = "+" | "-"

}
