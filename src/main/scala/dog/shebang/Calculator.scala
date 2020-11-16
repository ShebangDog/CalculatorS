package dog.shebang

import dog.shebang.Parser.expr

object Calculator {

  def main(args: Array[String]): Unit = {
    println(Parser.parseAll(expr, """(22 + 33) * 2""").getOrElse("error occurred"))
  }

}
