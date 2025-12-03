import scala.io.Source
import upickle.default.*
import java.io.{File, PrintWriter}

object Main {
  def main(args: Array[String]): Unit = {

    if (args.length == 0) {
      println("The Lua file is missing")
      System.exit(1)
    }

    val luaFilePath = args(0)

    val source = Source.fromFile(luaFilePath)
    val code = source.mkString
    source.close()

    val parser = Parser()
    val ast = parser.parse(code)

    val json = write(ast, indent = 2)

    // get filename without path and change extension to .json
    val inputFile = new File(luaFilePath)
    val baseName = inputFile.getName.replaceAll("\\.lua$", "")
    val outputDir = new File("JSON_output")
    outputDir.mkdirs()
    val outputFile = new File(outputDir, baseName + ".json")

    val writer = new PrintWriter(outputFile)
    writer.write(json)
    writer.close()

    println(s"Output written to ${outputFile.getPath}")
  }
}