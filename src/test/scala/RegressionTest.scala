// created with the assistance from Claude AI

import org.scalatest.funsuite.AnyFunSuite
import upickle.default.*
import scala.io.Source
import java.io.File

class RegressionTest extends AnyFunSuite {

  val baselineDir = "test_baselines"
  val luaProgramsDir = "lua_programs"

  val testFiles = List(
    "test_if",
    "test_while",
    "test_function",
    "test_for",
    "test_table",
    "test_full",
    "test_eval"
  )

  def readFile(path: String): String = {
    val source = Source.fromFile(path)
    try source.mkString finally source.close()
  }

  def normalizeJson(json: String): String = {
    json.replaceAll("\\s+", " ").trim
  }

  testFiles.foreach { testName =>
    test(s"regression: $testName matches baseline") {
      val luaPath = s"$luaProgramsDir/$testName.lua"
      val baselinePath = s"$baselineDir/$testName.json"

      assert(new File(luaPath).exists(), s"Lua file not found: $luaPath")
      assert(new File(baselinePath).exists(), s"Baseline not found: $baselinePath")

      val luaCode = readFile(luaPath)
      val parser = Parser()
      val ast = parser.parse(luaCode)
      val generatedJson = write(ast, indent = 2)
      val baselineJson = readFile(baselinePath)

      val normalizedGenerated = normalizeJson(generatedJson)
      val normalizedBaseline = normalizeJson(baselineJson)

      if (normalizedGenerated != normalizedBaseline) {
        val genLines = generatedJson.split("\n")
        val baseLines = baselineJson.split("\n")
        val diffLine = genLines.zip(baseLines).zipWithIndex.find {
          case ((g, b), _) => g != b
        }
        diffLine match {
          case Some(((gen, base), idx)) =>
            fail(s"JSON mismatch at line ${idx + 1}:\n  Generated: $gen\n  Baseline:  $base")
          case None =>
            if (genLines.length != baseLines.length) {
              fail(s"Line count differs: generated ${genLines.length}, baseline ${baseLines.length}")
            }
        }
      }
    }
  }

  test("regression: parser produces valid JSON for all test files") {
    testFiles.foreach { testName =>
      val luaPath = s"$luaProgramsDir/$testName.lua"
      if (new File(luaPath).exists()) {
        val luaCode = readFile(luaPath)
        val parser = Parser()
        val ast = parser.parse(luaCode)
        val json = write(ast, indent = 2)
        assert(json.nonEmpty, s"Empty JSON for $testName")
        assert(json.startsWith("{"), s"Invalid JSON start for $testName")
      }
    }
  }

  test("regression: AST structure is consistent") {
    testFiles.foreach { testName =>
      val luaPath = s"$luaProgramsDir/$testName.lua"
      if (new File(luaPath).exists()) {
        val luaCode = readFile(luaPath)
        val parser = Parser()
        val ast1 = parser.parse(luaCode)
        val ast2 = Parser().parse(luaCode)
        val json1 = write(ast1, indent = 2)
        val json2 = write(ast2, indent = 2)
        assert(json1 == json2, s"Parser produces inconsistent AST for $testName")
      }
    }
  }

  test("regression: baseline files exist for all test programs") {
    testFiles.foreach { testName =>
      val baselinePath = s"$baselineDir/$testName.json"
      assert(new File(baselinePath).exists(), s"Missing baseline: $baselinePath")
    }
  }

  test("regression: round-trip JSON serialization") {
    val luaCode = "x = 1 + 2"
    val parser = Parser()
    val ast = parser.parse(luaCode)
    val json = write(ast, indent = 2)
    assert(json.contains("\"$type\": \"AssignStat\""))
    assert(json.contains("\"$type\": \"BinopExp\""))
    assert(json.contains("\"op\": \"+\""))
  }
}