package dog.shebang.front

import dog.shebang.front.Evaluator.evalAsType
import dog.shebang.table.SymbolMap

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
      val result = typeCheck(t, evalAsType(value))

      result.foreach(_ => SymbolMap.declareValue(ident, value))
      AST.None

    case AST.FunctionDeclare(ident, argumentIdent, argumentType, typeInfo, body) =>
      val result = typeCheck(typeInfo, evalAsType(body))
      val function = AST.Function(argumentIdent, argumentType, typeInfo, body)

      result match {
        case Some(_) => SymbolMap.declareFunction(ident, function)
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
