package dog.shebang.table

import dog.shebang.front.{AST, Evaluator}

import scala.collection.mutable

object SymbolMap {

  private val hashMap: mutable.Map[String, AST.Expression] = mutable.Map()

  def declareValue(ident: String, value: AST.Expression): Unit = hashMap.update(ident, Evaluator.eval(value))

  def declareFunction(ident: String, function: AST.Function): Unit = hashMap.update(ident, function)

  def value(ident: String): Option[AST.Number] = hashMap.get(ident).map {
    case number: AST.Number => number
  }

  def function(ident: String, argumentType: AST.Type, argument: AST.Expression): Option[AST.Expression] = hashMap.get(ident)

}
