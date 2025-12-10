import scala.io.Source
import scala.util.{Success, Failure}  // might use later for better error handling
import upickle.default.*
import java.io.{File, PrintWriter}

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      println("Usage: run <file.lua>")
      System.exit(1)
    }

    val source = Source.fromFile(args(0))
    val code = source.mkString
    source.close()

    val parser = Parser()
    val ast = parser.parse(code)

    // write AST to JSON file
    val json = write(ast, indent = 2)
    // println(json)
    val inputFile = new File(args(0))
    val baseName = inputFile.getName.replaceAll("\\.lua$", "")
    val outputDir = new File("JSON_output")
    outputDir.mkdirs()
    val outputFile = new File(outputDir, baseName + ".json")
    val writer = new PrintWriter(outputFile)
    writer.write(json)
    writer.close()
    println(s"AST written to ${outputFile.getPath}")
    println()

    // run the program
    // println("starting eval...")
    val env = Eval.makeGlobalEnv()
    try {
      Eval.execBlock(ast.block, env)
    } catch {
      case e: EvalError => println(s"Error: ${e.getMessage}")
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}
