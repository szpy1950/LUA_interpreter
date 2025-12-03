// created with the assistance from Claude AI

import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source

class ParserTest extends AnyFunSuite {

  def parseFile(path: String): Chunk = {
    val source = Source.fromFile(path)
    val code = source.mkString
    source.close()
    val parser = Parser()
    parser.parse(code)
  }

  // x = 10
  // if x > 5 then y = "big" else y = "small" end
  test("parse test_if.lua") {
    val ast = parseFile("lua_programs/test_if.lua")

    // first statement: x = 10
    val assign = ast.block.stats(0).asInstanceOf[AssignStat]
    assert(assign.varlist(0).asInstanceOf[NameVar].name == "x")
    assert(assign.explist(0).asInstanceOf[Numeral].value == "10")

    // second statement: if x > 5 then...
    val ifstat = ast.block.stats(1).asInstanceOf[IfStat]
    val cond = ifstat.condition.asInstanceOf[BinopExp]
    assert(cond.op == ">")
    assert(cond.left.asInstanceOf[VarExp].v.asInstanceOf[NameVar].name == "x")
    assert(cond.right.asInstanceOf[Numeral].value == "5")
  }

  // i = 0
  // while i < 10 do i = i + 1 end
  test("parse test_while.lua") {
    val ast = parseFile("lua_programs/test_while.lua")

    // first statement: i = 0
    val assign = ast.block.stats(0).asInstanceOf[AssignStat]
    assert(assign.varlist(0).asInstanceOf[NameVar].name == "i")
    assert(assign.explist(0).asInstanceOf[Numeral].value == "0")

    // second statement: while i < 10 do...
    val whilestat = ast.block.stats(1).asInstanceOf[WhileStat]
    val cond = whilestat.condition.asInstanceOf[BinopExp]
    assert(cond.op == "<")
    assert(cond.right.asInstanceOf[Numeral].value == "10")
  }

  // function add(a, b) return a + b end
  // result = add(3, 5)
  test("parse test_function.lua") {
    val ast = parseFile("lua_programs/test_function.lua")

    // first statement: function add(a, b)
    val func = ast.block.stats(0).asInstanceOf[FunctionStat]
    assert(func.name.names(0) == "add")
    assert(func.body.params(0) == "a")
    assert(func.body.params(1) == "b")

    // check return statement inside function
    val retstat = func.body.block.retstat.get
    val binop = retstat.explist(0).asInstanceOf[BinopExp]
    assert(binop.op == "+")
  }

  // person = { name = "Alice", age = 30 }
  // x = person.name
  // y = person["age"]
  test("parse test_table.lua") {
    val ast = parseFile("lua_programs/test_table.lua")

    // first statement: person = { name = "Alice", age = 30 }
    val assign1 = ast.block.stats(0).asInstanceOf[AssignStat]
    assert(assign1.varlist(0).asInstanceOf[NameVar].name == "person")
    val table = assign1.explist(0).asInstanceOf[TableConstructor]
    assert(table.fields.length == 2)

    // second statement: x = person.name (dot access)
    val assign2 = ast.block.stats(1).asInstanceOf[AssignStat]
    val dotvar = assign2.explist(0).asInstanceOf[VarExp].v.asInstanceOf[DotVar]
    assert(dotvar.name == "name")

    // third statement: y = person["age"] (bracket access)
    val assign3 = ast.block.stats(2).asInstanceOf[AssignStat]
    val indexvar = assign3.explist(0).asInstanceOf[VarExp].v.asInstanceOf[IndexVar]
    assert(indexvar.index.asInstanceOf[LiteralString].value == "age")
  }

  // for i = 1, 10, 1 do x = i * 2 end
  // for k, v in pairs(t) do print(k, v) end
  test("parse test_for.lua") {
    val ast = parseFile("lua_programs/test_for.lua")

    // first statement: for i = 1, 10, 1 do
    val fornum = ast.block.stats(0).asInstanceOf[ForNumStat]
    assert(fornum.name == "i")
    assert(fornum.start.asInstanceOf[Numeral].value == "1")
    assert(fornum.end.asInstanceOf[Numeral].value == "10")
    assert(fornum.step.get.asInstanceOf[Numeral].value == "1")

    // second statement: for k, v in pairs(t) do
    val forin = ast.block.stats(1).asInstanceOf[ForInStat]
    assert(forin.names(0) == "k")
    assert(forin.names(1) == "v")
  }
}