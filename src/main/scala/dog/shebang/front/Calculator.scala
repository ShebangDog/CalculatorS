package dog.shebang.front

import dog.shebang.front.Parser.program

object Calculator {

  def main(args: Array[String]): Unit = {
    Parser.parseAll(program,
      """
        | fun function(value: Int): Int = { value }
        |
        | print(function(100 + 222))
        |
        | """.stripMargin).get
      .map(SemanticAnalyzer.analyze)
      .foreach(Emitter.emit)
  }

}
