package dog.shebang.front

import dog.shebang.front.AST.FunctionDeclare
import dog.shebang.table.SymbolMap

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {
  def program: Parser[List[AST.Statement]] = rep(statement ~ rep("""\n""")) ^^ { program =>
    program.foldRight(List(AST.None): List[AST.Statement]) { case (line ~ _, res) => line :: res }
  }

  def statement: Parser[AST.Statement] = "val" ~ ident ~ ":" ~ type_info ~ "=" ~ expr ^^ {
    case _ ~ id ~ _ ~ t ~ _ ~ exp =>
      SymbolMap.declareValue(id, exp)
      AST.ValueDeclare(id, exp, t)
  } |
    "print" ~ "(" ~> expr <~ ")" ^^ (exp => AST.Print(exp)) |
    "println" ~ "(" ~> expr <~ ")" ^^ (exp => AST.Println(exp)) |
    ("fun" ~> ident <~ "(") ~ (opt(parameters) <~ ")" ~ ":") ~ (type_info <~ "=") ~ body ^^ {
      case funcId ~ optionParameters ~ t ~ body =>
        optionParameters.map(parameters => {
          val parameterIds = parameters.map(_._1)
          val function = AST.Function(parameterIds, body)

          SymbolMap.declareFunction(funcId, function)
          FunctionDeclare(funcId, parameters, t, body)
        }).get
    } |
    expr ^^ AST.Line

  def parameters: Parser[List[(String, AST.Type)]] = parameter ~ rep("," ~> parameter) ^^ {
    case param ~ paramList => param :: paramList
  }

  def parameter: Parser[(String, AST.Type)] = (ident <~ ":") ~ type_info ^^ { case id ~ t => (id, t) }

  def body: Parser[List[AST.Expression]] = "{" ~> rep(rep("""\n""") ~> expr <~ rep("""\n""")) <~ "}" ^^ (exprList => exprList)

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
    "(" ~> expr <~ ")" ^^ (num => num) |
    (ident <~ "(") ~ (opt(arguments) <~ ")") ^^ {
      case id ~ Some(arguments) =>
        SymbolMap.function(id).map {
          case AST.Function(argumentIds, body) =>
            (argumentIds zip arguments).foreach { case (id, exp) => SymbolMap.declareValue(id, exp) }

            Evaluator.eval(AST.Function(argumentIds, body))
        }.get
    } |
    ident ^^ AST.Value

  def arguments: Parser[List[AST.Expression]] = argument ~ rep("," ~> argument) ^^ {
    case argument ~ argumentList => argument :: argumentList
  }

  def argument: Parser[AST.Expression] = expr

  def type_info: Parser[AST.Type] = ident ^^ {
    case AST.Int.typeName => AST.Int
    case AST.Double.typeName => AST.Double
    case _ => ???
  }

  def primary_operator: Parser[String] = "*" | "/"

  def secondary_operator: Parser[String] = "+" | "-"
}
