package dog.shebang.front

object Emitter {

  def emit(statement: AST.Statement): Unit = statement match {
    case _: AST.Declare => "None"
    case AST.Print(value) => print(Evaluator.eval(value).makeString())
    case AST.Println(value) => println(Evaluator.eval(value).makeString())
    case AST.Line(value) => Evaluator.eval(value)
    case AST.None => "None"
  }
}
