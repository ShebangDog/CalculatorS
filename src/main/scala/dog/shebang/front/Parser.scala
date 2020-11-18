package dog.shebang.front

import dog.shebang.table.SymbolMap

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {
  def program: Parser[List[AST.Statement]] = rep(statement ~ rep("""\n""")) ^^ { program =>
    program.foldRight(List(AST.None): List[AST.Statement]) { case (line ~ _, res) => line :: res }
  }

  def statement: Parser[AST.Statement] = "val" ~ ident ~ ":" ~ type_info ~ "=" ~ expr ^^ {
    case _ ~ id ~ _ ~ t ~ _ ~ exp =>
      SymbolMap.declareIdent(id, exp)
      AST.Declare(id, exp, t)
  } |
    "print" ~ "(" ~ expr ~ ")" ^^ { case _ ~ _ ~ exp ~ _ => AST.Print(exp) } |
    "println" ~ "(" ~ expr ~ ")" ^^ { case _ ~ _ ~ exp ~ _ => AST.Println(exp) } |
    expr ^^ AST.Line

  def expr: Parser[AST.Expression] = (term ~ rep(secondary_operator ~ term)) ^^ {
    case value ~ Nil => value
    case t ~ rest => rest.foldLeft(t) {
      case (l, "+" ~ r) => AST.Addition(l, r)
      case (l, "-" ~ r) => AST.Subtraction(l, r)
    }
  }

  def term: Parser[AST.Expression] = factor ~ rep(primary_operator ~ factor) ^^ {
    case fact ~ Nil => fact
    case fact ~ rest => rest.foldLeft(fact) {
      case (l, "*" ~ r) => AST.Multiplication(l, r)
      case (l, "/" ~ r) => AST.Division(l, r)
    }
  }

  def factor: Parser[AST.Expression] = wholeNumber ^^ (num => AST.IntNumber(num.toInt)) |
    floatingPointNumber ^^ (floatNum => AST.DoubleNumber(floatNum.toDouble)) |
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
