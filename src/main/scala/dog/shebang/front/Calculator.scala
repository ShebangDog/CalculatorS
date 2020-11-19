package dog.shebang.front

import dog.shebang.front.Parser.program

object Calculator {

  def main(args: Array[String]): Unit = {
    Parser.parseAll(program,
      """
        |
        |
        | val double: Double = 12.0
        | val int: Int = 12
        | val result: Double = double + int
        | 
        | print(result)
        |
        | """.stripMargin).get
      .map(SemanticAnalyzer.analyze)
      .foreach(Emitter.emit)
  }

}
