package dog.shebang.front

object AST {

  sealed abstract class Node


  sealed abstract class Statement extends Node

  case class Declare(ident: String, value: Expression, typeInfo: Type) extends Statement

  case class Line(value: Expression) extends Statement

  case class Print(value: Expression) extends Statement

  case class Println(value: Expression) extends Statement

  case object None extends Statement


  sealed abstract class Expression extends Node


  sealed abstract class Number extends Expression {

    def +(other: Number): Number

    def -(other: Number): Number

    def *(other: Number): Number

    def /(other: Number): Number

    def makeString(): String
  }

  case class IntNumber(value: Int) extends Number {
    override def +(other: Number): Number = other match {
      case IntNumber(int) => IntNumber(value + int)
      case DoubleNumber(double) => DoubleNumber(value + double)
    }

    override def -(other: Number): Number = other match {
      case IntNumber(int) => IntNumber(value - int)
      case DoubleNumber(double) => DoubleNumber(value - double)
    }

    override def *(other: Number): Number = other match {
      case IntNumber(int) => IntNumber(value * int)
      case DoubleNumber(double) => DoubleNumber(value * double)
    }

    override def /(other: Number): Number = other match {
      case IntNumber(int) => IntNumber(value / int)
      case DoubleNumber(double) => DoubleNumber(value / double)
    }

    override def makeString(): String = value.toString
  }

  case class DoubleNumber(value: Double) extends Number {
    override def +(other: Number): Number = other match {
      case IntNumber(int) => DoubleNumber(value + int)
      case DoubleNumber(double) => DoubleNumber(value + double)
    }

    override def -(other: Number): Number = other match {
      case IntNumber(int) => DoubleNumber(value - int)
      case DoubleNumber(double) => DoubleNumber(value - double)
    }

    override def *(other: Number): Number = other match {
      case IntNumber(int) => DoubleNumber(value * int)
      case DoubleNumber(double) => DoubleNumber(value * double)
    }

    override def /(other: Number): Number = other match {
      case IntNumber(int) => DoubleNumber(value / int)
      case DoubleNumber(double) => DoubleNumber(value / double)
    }

    override def makeString(): String = value.toString
  }

  sealed abstract class Arithmetic(val operator: String) extends Expression

  case class Addition(left: Expression, right: Expression) extends Arithmetic("+")

  case class Subtraction(left: Expression, right: Expression) extends Arithmetic("-")

  case class Multiplication(left: Expression, right: Expression) extends Arithmetic("*")

  case class Division(left: Expression, right: Expression) extends Arithmetic("/")


  sealed abstract class Type(val typeName: String) extends Node

  case object Int extends Type("Int")

  case object Double extends Type("Double")

}
