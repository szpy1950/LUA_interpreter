import scala.io.Source
import scala.util.{Success, Failure}  // might use later for better error handling
import upickle.default.*
import java.io.{File, PrintWriter}

object Main {
  /** Entry point: uses a lua file path as argument and parses it into an Abstract Syntaxe Tree (AST)
   * then writes the AST to JSON for representation, finally executes the program
   */
  def main(args: Array[String]): Unit = {

    /**
     * The first argument is the lua file path
     */
    if (args.length == 0) {
      println("Usage: run <file.lua>")
      System.exit(1)
    }

    /**
     * Reads cdoe from path and turns it into a single string
     */
    val source = Source.fromFile(args(0))
    val code = source.mkString
    source.close()

    /**
     * Parse the source code into an Abstract Syntaxe tree
     * */
    val parser = Parser()
    val ast = parser.parse(code)

    /**
     * Transform the AST into JSON text and save it in the JSON_output directory
     */
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

    /** Create global environment with built-in function :
     * - print
     * - type
     * - tonumber
     * - tostring
     * - error
     * then executes the program
     */
    val env = Eval.makeGlobalEnv()
    try {
      Eval.execBlock(ast.block, env)
    } catch {
      /** There are two types of errors: EvalError ( Lua runtime errors ) and
       * general exceptions ( parser errors )
       */
      case e: EvalError => println(s"Error: ${e.getMessage}")
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}
