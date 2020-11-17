package dog.shebang

trait AST

case class Number(value: Double) extends AST
case class Node(operator: String, factors: Seq[AST]) extends AST
