package dog.shebang.front

import dog.shebang.front.Evaluator.evalAsType

object SemanticAnalyzer {
  def analyze(statement: AST.Statement): AST.Statement = statement match {
    case declare: AST.Declare => analyze(declare)
    case AST.Line(_) => AST.None
    case AST.Print(_) => statement
    case AST.Println(_) => statement
    case AST.None => statement
  }

  private def analyze(declare: AST.Declare): AST.Statement = declare match {
    case AST.ValueDeclare(ident, value, t) =>
      typeCheck(t, evalAsType(value)) match {
        case Some(_) =>
        case None => semanticError(ident, t)
      }
      AST.None

    case AST.FunctionDeclare(ident, _, _, typeInfo, body) =>
      typeCheck(typeInfo, evalAsType(body)) match {
        case Some(_) =>
        case None => semanticError(ident, typeInfo)
      }
      AST.None
  }

  private def typeCheck(typeLeft: AST.Type, typeRight: AST.Type): Option[AST.Type] = typeLeft match {
    case AST.Int => if (typeRight == AST.Double) None else Option(typeRight)
    case AST.Double => Option(typeRight)
  }

  private def semanticError(ident: String, incorrectType: AST.Type): Unit = println(ident + " is not " + incorrectType.typeName)
}
