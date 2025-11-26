import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {

    // Test if we have an argument
    if (args.length == 0) {
      println("The Lua file is missing")
      System.exit(1)
    }

    // First argument becomes the filepath
    val luaFilePath = args(0)

    val source = Source.fromFile(luaFilePath)
    val code = source.mkString
    source.close()

//    println("LUA code: ")
//    println(code)

    val parser = Parser()
    val ast = parser.parse(code)

    println(ast.toJson())

  }
}