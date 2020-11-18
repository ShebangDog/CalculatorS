package dog.shebang

object AST {

  sealed abstract class Node


  sealed abstract class Statement extends Node

  case class Declare(ident: String, value: Expression) extends Statement

  case class Line(value: Expression) extends Statement

  case object None extends Statement

  sealed abstract class Expression extends Node

  case class Number(value: Double) extends Expression


  sealed abstract class Arithmetic(val operator: String) extends Expression

  case class Addition(left: Expression, right: Expression) extends Arithmetic("+")

  case class Subtraction(left: Expression, right: Expression) extends Arithmetic("-")

  case class Multiplication(left: Expression, right: Expression) extends Arithmetic("*")

  case class Division(left: Expression, right: Expression) extends Arithmetic("/")

}
