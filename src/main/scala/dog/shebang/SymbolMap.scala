package dog.shebang

import dog.shebang.AST.Expression

import scala.collection.mutable

object SymbolMap {

  private val hashMap: mutable.Map[String, AST.Number] = mutable.Map()

  def declareIdent(ident: String, value: Expression): Unit = hashMap.update(ident, Calculator.eval(value))

  def getIdent(ident: String): Option[AST.Number] = hashMap.get(ident)
}
