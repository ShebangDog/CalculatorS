package dog.shebang.front

import dog.shebang.front.Parser.program

object Calculator {

  def main(args: Array[String]): Unit = {
    Parser.parseAll(program,
      """
        | fun function(value: Int, hoge: Int): Int = {
        |   value + hoge
        |   value
        |   1212
        | }
        |
        | print(hoge + 222 + function(111, 121))
        |
        | """.stripMargin).get
      .map(SemanticAnalyzer.analyze)
      .foreach(Emitter.emit)
  }

}
