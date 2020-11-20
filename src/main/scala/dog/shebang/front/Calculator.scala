package dog.shebang.front

import dog.shebang.front.Parser.program

object Calculator {

  def main(args: Array[String]): Unit = {
    Parser.parseAll(program,
      """
        | fun function(value: Int): Int = { value + hoge }
        |
        | val hoge:Int = 1
        |
        | print(hoge + 222 + function(111))
        |
        | """.stripMargin).get
      .map(SemanticAnalyzer.analyze)
      .foreach(Emitter.emit)
  }

}
