package dog.shebang.table

import dog.shebang.front.{AST, Evaluator}

import scala.collection.mutable

object SymbolMap {

  private val hashMap: mutable.Map[String, AST.Number] = mutable.Map()

  def declareIdent(ident: String, value: AST.Expression): Unit = hashMap.update(ident, Evaluator.eval(value))

  def getIdent(ident: String): Option[AST.Number] = hashMap.get(ident)
}
