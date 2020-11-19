package dog.shebang.front

import dog.shebang.front.Evaluator.eval

object SemanticAnalyzer {
  def analyze(statement: AST.Statement): AST.Statement = statement match {
    case declare: AST.Declare => analyze(declare)
    case AST.Line(_) => AST.None
    case AST.Print(_) => statement
    case AST.Println(_) => statement
    case AST.None => statement
  }

  private def analyze(declare: AST.Declare): AST.Statement = declare match {
    case AST.Declare(_, value, t) => eval(value) match {
      case AST.IntNumber(_) => t match {
        case AST.Int => AST.None
        case _ =>
          semanticError(declare, t)
          AST.None
      }
      case AST.DoubleNumber(_) => t match {
        case AST.Double => AST.None
        case _ =>
          semanticError(declare, t)
          AST.None
      }
    }
  }

  private def semanticError(declare: AST.Declare, incorrectType: AST.Type): Unit = declare match {
    case AST.Declare(ident, _, _) => println(ident + " is not " + incorrectType.typeName)
  }
}
