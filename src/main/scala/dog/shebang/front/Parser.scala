package dog.shebang.front

import dog.shebang.front.AST.FunctionDeclare
import dog.shebang.table.SymbolMap

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {
  def program: Parser[List[AST.Statement]] = rep(statement ~ rep("""\n""")) ^^ { program =>
    program.foldRight(List(AST.None): List[AST.Statement]) { case (line ~ _, res) => line :: res }
  }

  def statement: Parser[AST.Statement] = "val" ~ ident ~ ":" ~ type_info ~ "=" ~ expr ^^ {
    case _ ~ id ~ _ ~ t ~ _ ~ exp => AST.ValueDeclare(id, exp, t)
  } |
    "print" ~ "(" ~ expr ~ ")" ^^ { case _ ~ _ ~ exp ~ _ => AST.Print(exp) } |
    "println" ~ "(" ~ expr ~ ")" ^^ { case _ ~ _ ~ exp ~ _ => AST.Println(exp) } |
    "fun" ~ ident ~ "(" ~ ident ~ ":" ~ type_info ~ ")" ~ ":" ~ type_info ~ "=" ~ body ^^ {
      case _ ~ funcId ~ _ ~ argId ~ _ ~ argType ~ _ ~ _ ~ t ~ _ ~ body =>
        val function = AST.Function(argId, body)

        SymbolMap.declareFunction(funcId, function)
        FunctionDeclare(funcId, argId, argType, t, body)
    } |
    expr ^^ AST.Line

  def body: Parser[AST.Expression] = "{" ~ expr ~ "}" ^^ {
    case _ ~ exp ~ _ => exp
  }

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

  def factor: Parser[AST.Expression] = floatingPointNumber ^^ { numberString =>
    if (numberString.contains('.')) AST.DoubleNumber(numberString.toDouble)
    else AST.IntNumber(numberString.toInt)
  } |
    "(" ~ expr ~ ")" ^^ { case "(" ~ num ~ ")" => num } |
    ident ~ "(" ~ expr ~ ")" ^^ {
      case functionName ~ _ ~ argument ~ _ =>
        val function = SymbolMap.function(functionName)

        function.map {
          case AST.Function(argId, body) =>
            SymbolMap.declareValue(argId, argument)
            Evaluator.eval(AST.Function(argId, body))
        }.get
    } |
    ident ^^ AST.Value

  def type_info: Parser[AST.Type] = ident ^^ {
    case AST.Int.typeName => AST.Int
    case AST.Double.typeName => AST.Double
    case _ => ???
  }

  def primary_operator: Parser[String] = "*" | "/"

  def secondary_operator: Parser[String] = "+" | "-"
}
