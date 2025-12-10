// created with the assistance from Claude AI

import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source

class ParserTest extends AnyFunSuite {

  // helper to parse code string
  def parse(code: String): Chunk = {
    val parser = Parser()
    parser.parse(code)
  }

  // helper to parse file
  def parseFile(path: String): Chunk = {
    val source = Source.fromFile(path)
    val code = source.mkString
    source.close()
    parse(code)
  }

  // ============================================================================
  // LITERALS
  // ============================================================================

  test("literal: nil") {
    val ast = parse("x = nil")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.isInstanceOf[NilExp])
  }

  test("literal: true") {
    val ast = parse("x = true")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.isInstanceOf[TrueExp])
  }

  test("literal: false") {
    val ast = parse("x = false")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.isInstanceOf[FalseExp])
  }

  test("literal: number integer") {
    val ast = parse("x = 42")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[Numeral].value == "42")
  }

  test("literal: number float") {
    val ast = parse("x = 3.14")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[Numeral].value == "3.14")
  }

  test("literal: string double quotes") {
    val ast = parse("x = \"hello\"")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[LiteralString].value == "hello")
  }

  test("literal: string single quotes") {
    val ast = parse("x = 'world'")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[LiteralString].value == "world")
  }

  // ============================================================================
  // VARIABLES
  // ============================================================================

  test("var: simple name") {
    val ast = parse("x = y")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val v = exp.asInstanceOf[VarExp].v.asInstanceOf[NameVar]
    assert(v.name == "y")
  }

  test("var: dot access") {
    val ast = parse("x = t.name")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val v = exp.asInstanceOf[VarExp].v.asInstanceOf[DotVar]
    assert(v.name == "name")
  }

  test("var: bracket access") {
    val ast = parse("x = t[1]")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val v = exp.asInstanceOf[VarExp].v.asInstanceOf[IndexVar]
    assert(v.index.asInstanceOf[Numeral].value == "1")
  }

  test("var: bracket access with string") {
    val ast = parse("x = t[\"key\"]")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val v = exp.asInstanceOf[VarExp].v.asInstanceOf[IndexVar]
    assert(v.index.asInstanceOf[LiteralString].value == "key")
  }

  test("var: chained dot access") {
    val ast = parse("x = a.b.c")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val v = exp.asInstanceOf[VarExp].v.asInstanceOf[DotVar]
    assert(v.name == "c")
  }

  // ============================================================================
  // BINARY OPERATORS
  // ============================================================================

  test("binop: addition") {
    val ast = parse("x = 1 + 2")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val binop = exp.asInstanceOf[BinopExp]
    assert(binop.op == "+")
  }

  test("binop: subtraction") {
    val ast = parse("x = 5 - 3")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == "-")
  }

  test("binop: multiplication") {
    val ast = parse("x = 2 * 3")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == "*")
  }

  test("binop: division") {
    val ast = parse("x = 10 / 2")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == "/")
  }

  test("binop: floor division") {
    val ast = parse("x = 7 // 3")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == "//")
  }

  test("binop: modulo") {
    val ast = parse("x = 10 % 3")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == "%")
  }

  test("binop: power") {
    val ast = parse("x = 2 ^ 3")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == "^")
  }

  test("binop: concat") {
    val ast = parse("x = \"a\" .. \"b\"")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == "..")
  }

  test("binop: less than") {
    val ast = parse("x = 1 < 2")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == "<")
  }

  test("binop: greater than") {
    val ast = parse("x = 2 > 1")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == ">")
  }

  test("binop: less or equal") {
    val ast = parse("x = 1 <= 2")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == "<=")
  }

  test("binop: greater or equal") {
    val ast = parse("x = 2 >= 1")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == ">=")
  }

  test("binop: equal") {
    val ast = parse("x = 1 == 1")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == "==")
  }

  test("binop: not equal") {
    val ast = parse("x = 1 ~= 2")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == "~=")
  }

  test("binop: and") {
    val ast = parse("x = true and false")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == "and")
  }

  test("binop: or") {
    val ast = parse("x = true or false")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[BinopExp].op == "or")
  }

  // ============================================================================
  // UNARY OPERATORS
  // ============================================================================

  test("unop: minus") {
    val ast = parse("x = -5")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[UnopExp].op == "-")
  }

  test("unop: not") {
    val ast = parse("x = not true")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[UnopExp].op == "not")
  }

  test("unop: length") {
    val ast = parse("x = #t")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[UnopExp].op == "#")
  }

  test("unop: bitwise not") {
    val ast = parse("x = ~5")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.asInstanceOf[UnopExp].op == "~")
  }

  // ============================================================================
  // OPERATOR PRECEDENCE
  // ============================================================================

  test("precedence: mult before add") {
    val ast = parse("x = 1 + 2 * 3")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val binop = exp.asInstanceOf[BinopExp]
    assert(binop.op == "+")
    assert(binop.right.asInstanceOf[BinopExp].op == "*")
  }

  test("precedence: power before mult") {
    val ast = parse("x = 2 * 3 ^ 2")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val binop = exp.asInstanceOf[BinopExp]
    assert(binop.op == "*")
    assert(binop.right.asInstanceOf[BinopExp].op == "^")
  }

  test("precedence: unary before binary") {
    val ast = parse("x = -2 ^ 2")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val unop = exp.asInstanceOf[UnopExp]
    assert(unop.op == "-")
  }

  test("precedence: parentheses") {
    val ast = parse("x = (1 + 2) * 3")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val binop = exp.asInstanceOf[BinopExp]
    assert(binop.op == "*")
    assert(binop.left.isInstanceOf[ParenExp])
  }

  // ============================================================================
  // ASSIGNMENT STATEMENTS
  // ============================================================================

  test("assign: single") {
    val ast = parse("x = 10")
    val stat = ast.block.stats.head.asInstanceOf[AssignStat]
    assert(stat.varlist.length == 1)
    assert(stat.explist.length == 1)
  }

  test("assign: multiple vars") {
    val ast = parse("a, b, c = 1, 2, 3")
    val stat = ast.block.stats.head.asInstanceOf[AssignStat]
    assert(stat.varlist.length == 3)
    assert(stat.explist.length == 3)
  }

  test("assign: to table field") {
    val ast = parse("t.x = 10")
    val stat = ast.block.stats.head.asInstanceOf[AssignStat]
    assert(stat.varlist.head.isInstanceOf[DotVar])
  }

  test("assign: to table index") {
    val ast = parse("t[1] = 10")
    val stat = ast.block.stats.head.asInstanceOf[AssignStat]
    assert(stat.varlist.head.isInstanceOf[IndexVar])
  }

  // ============================================================================
  // LOCAL STATEMENTS
  // ============================================================================

  test("local: single var") {
    val ast = parse("local x = 10")
    val stat = ast.block.stats.head.asInstanceOf[LocalStat]
    assert(stat.names.length == 1)
    assert(stat.names.head == "x")
  }

  test("local: multiple vars") {
    val ast = parse("local a, b, c = 1, 2, 3")
    val stat = ast.block.stats.head.asInstanceOf[LocalStat]
    assert(stat.names.length == 3)
  }

  test("local: without initialization") {
    val ast = parse("local x")
    val stat = ast.block.stats.head.asInstanceOf[LocalStat]
    assert(stat.names.head == "x")
    assert(stat.explist.isEmpty)
  }

  // ============================================================================
  // IF STATEMENTS
  // ============================================================================

  test("if: simple") {
    val ast = parse("if true then x = 1 end")
    val stat = ast.block.stats.head.asInstanceOf[IfStat]
    assert(stat.condition.isInstanceOf[TrueExp])
    assert(stat.elseifs.isEmpty)
    assert(stat.elseBlock.isEmpty)
  }

  test("if: with else") {
    val ast = parse("if true then x = 1 else x = 2 end")
    val stat = ast.block.stats.head.asInstanceOf[IfStat]
    assert(stat.elseBlock.isDefined)
  }

  test("if: with elseif") {
    val ast = parse("if x > 10 then y = 1 elseif x > 5 then y = 2 end")
    val stat = ast.block.stats.head.asInstanceOf[IfStat]
    assert(stat.elseifs.length == 1)
  }

  test("if: with multiple elseif") {
    val ast = parse("if x > 10 then y = 1 elseif x > 5 then y = 2 elseif x > 0 then y = 3 end")
    val stat = ast.block.stats.head.asInstanceOf[IfStat]
    assert(stat.elseifs.length == 2)
  }

  test("if: with elseif and else") {
    val ast = parse("if x > 10 then y = 1 elseif x > 5 then y = 2 else y = 3 end")
    val stat = ast.block.stats.head.asInstanceOf[IfStat]
    assert(stat.elseifs.length == 1)
    assert(stat.elseBlock.isDefined)
  }

  // ============================================================================
  // WHILE STATEMENTS
  // ============================================================================

  test("while: simple") {
    val ast = parse("while x < 10 do x = x + 1 end")
    val stat = ast.block.stats.head.asInstanceOf[WhileStat]
    assert(stat.condition.asInstanceOf[BinopExp].op == "<")
  }

  test("while: with break") {
    val ast = parse("while true do break end")
    val stat = ast.block.stats.head.asInstanceOf[WhileStat]
    assert(stat.block.stats.head.isInstanceOf[BreakStat])
  }

  // ============================================================================
  // REPEAT STATEMENTS
  // ============================================================================

  test("repeat: simple") {
    val ast = parse("repeat x = x + 1 until x >= 10")
    val stat = ast.block.stats.head.asInstanceOf[RepeatStat]
    assert(stat.condition.asInstanceOf[BinopExp].op == ">=")
  }

  // ============================================================================
  // FOR STATEMENTS
  // ============================================================================

  test("for: numeric simple") {
    val ast = parse("for i = 1, 10 do end")
    val stat = ast.block.stats.head.asInstanceOf[ForNumStat]
    assert(stat.name == "i")
    assert(stat.start.asInstanceOf[Numeral].value == "1")
    assert(stat.end.asInstanceOf[Numeral].value == "10")
    assert(stat.step.isEmpty)
  }

  test("for: numeric with step") {
    val ast = parse("for i = 1, 10, 2 do end")
    val stat = ast.block.stats.head.asInstanceOf[ForNumStat]
    assert(stat.step.isDefined)
    assert(stat.step.get.asInstanceOf[Numeral].value == "2")
  }

  test("for: generic single var") {
    val ast = parse("for k in pairs(t) do end")
    val stat = ast.block.stats.head.asInstanceOf[ForInStat]
    assert(stat.names.length == 1)
    assert(stat.names.head == "k")
  }

  test("for: generic multiple vars") {
    val ast = parse("for k, v in pairs(t) do end")
    val stat = ast.block.stats.head.asInstanceOf[ForInStat]
    assert(stat.names.length == 2)
    assert(stat.names(0) == "k")
    assert(stat.names(1) == "v")
  }

  // ============================================================================
  // FUNCTION DEFINITIONS
  // ============================================================================

  test("function: global simple") {
    val ast = parse("function foo() end")
    val stat = ast.block.stats.head.asInstanceOf[FunctionStat]
    assert(stat.name.names.head == "foo")
    assert(stat.body.params.isEmpty)
  }

  test("function: with params") {
    val ast = parse("function foo(a, b) end")
    val stat = ast.block.stats.head.asInstanceOf[FunctionStat]
    assert(stat.body.params.length == 2)
    assert(stat.body.params(0) == "a")
    assert(stat.body.params(1) == "b")
  }

  test("function: with vararg") {
    val ast = parse("function foo(...) end")
    val stat = ast.block.stats.head.asInstanceOf[FunctionStat]
    assert(stat.body.vararg)
  }

  test("function: with params and vararg") {
    val ast = parse("function foo(a, b, ...) end")
    val stat = ast.block.stats.head.asInstanceOf[FunctionStat]
    assert(stat.body.params.length == 2)
    assert(stat.body.vararg)
  }

  test("function: method syntax") {
    val ast = parse("function obj:method() end")
    val stat = ast.block.stats.head.asInstanceOf[FunctionStat]
    assert(stat.name.names.head == "obj")
    assert(stat.name.method.contains("method"))
  }

  test("function: nested name") {
    val ast = parse("function a.b.c() end")
    val stat = ast.block.stats.head.asInstanceOf[FunctionStat]
    assert(stat.name.names.length == 3)
  }

  test("function: local") {
    val ast = parse("local function foo() end")
    val stat = ast.block.stats.head.asInstanceOf[LocalFunctionStat]
    assert(stat.name == "foo")
  }

  test("function: anonymous") {
    val ast = parse("f = function() end")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    assert(exp.isInstanceOf[FunctionDefExp])
  }

  test("function: with return") {
    val ast = parse("function foo() return 1 end")
    val stat = ast.block.stats.head.asInstanceOf[FunctionStat]
    assert(stat.body.block.retstat.isDefined)
  }

  test("function: with multiple return") {
    val ast = parse("function foo() return 1, 2, 3 end")
    val stat = ast.block.stats.head.asInstanceOf[FunctionStat]
    assert(stat.body.block.retstat.get.explist.length == 3)
  }

  // ============================================================================
  // FUNCTION CALLS
  // ============================================================================

  test("call: simple") {
    val ast = parse("foo()")
    val stat = ast.block.stats.head.asInstanceOf[FunctionCallStat]
    assert(stat.call.args.isEmpty)
  }

  test("call: with args") {
    val ast = parse("foo(1, 2, 3)")
    val stat = ast.block.stats.head.asInstanceOf[FunctionCallStat]
    assert(stat.call.args.length == 3)
  }

  test("call: method") {
    val ast = parse("obj:method()")
    val stat = ast.block.stats.head.asInstanceOf[FunctionCallStat]
    assert(stat.call.method.contains("method"))
  }

  test("call: string argument") {
    val ast = parse("print \"hello\"")
    val stat = ast.block.stats.head.asInstanceOf[FunctionCallStat]
    assert(stat.call.args.head.isInstanceOf[LiteralString])
  }

  test("call: table argument") {
    val ast = parse("foo{1, 2, 3}")
    val stat = ast.block.stats.head.asInstanceOf[FunctionCallStat]
    assert(stat.call.args.head.isInstanceOf[TableConstructor])
  }

  test("call: chained") {
    val ast = parse("foo()()")
    val stat = ast.block.stats.head.asInstanceOf[FunctionCallStat]
    assert(stat.call.prefix.isInstanceOf[FunctionCallExp])
  }

  // ============================================================================
  // TABLE CONSTRUCTORS
  // ============================================================================

  test("table: empty") {
    val ast = parse("t = {}")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val table = exp.asInstanceOf[TableConstructor]
    assert(table.fields.isEmpty)
  }

  test("table: array style") {
    val ast = parse("t = {1, 2, 3}")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val table = exp.asInstanceOf[TableConstructor]
    assert(table.fields.length == 3)
    assert(table.fields.head.isInstanceOf[ValueField])
  }

  test("table: record style") {
    val ast = parse("t = {x = 1, y = 2}")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val table = exp.asInstanceOf[TableConstructor]
    assert(table.fields.length == 2)
    assert(table.fields.head.isInstanceOf[NameKeyField])
  }

  test("table: computed key") {
    val ast = parse("t = {[1+1] = 2}")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val table = exp.asInstanceOf[TableConstructor]
    assert(table.fields.head.isInstanceOf[ExpKeyField])
  }

  test("table: mixed") {
    val ast = parse("t = {1, x = 2, [3] = 4}")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val table = exp.asInstanceOf[TableConstructor]
    assert(table.fields.length == 3)
  }

  test("table: with trailing comma") {
    val ast = parse("t = {1, 2, 3,}")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val table = exp.asInstanceOf[TableConstructor]
    assert(table.fields.length == 3)
  }

  test("table: with semicolon separator") {
    val ast = parse("t = {1; 2; 3}")
    val exp = ast.block.stats.head.asInstanceOf[AssignStat].explist.head
    val table = exp.asInstanceOf[TableConstructor]
    assert(table.fields.length == 3)
  }

  // ============================================================================
  // DO BLOCK
  // ============================================================================

  test("do: simple") {
    val ast = parse("do x = 1 end")
    val stat = ast.block.stats.head.asInstanceOf[DoStat]
    assert(stat.block.stats.length == 1)
  }

  // ============================================================================
  // BREAK AND GOTO
  // ============================================================================

  test("break: simple") {
    val ast = parse("while true do break end")
    val whilestat = ast.block.stats.head.asInstanceOf[WhileStat]
    assert(whilestat.block.stats.head.isInstanceOf[BreakStat])
  }

  test("goto: simple") {
    val ast = parse("goto label")
    val stat = ast.block.stats.head.asInstanceOf[GotoStat]
    assert(stat.name == "label")
  }

  test("label: simple") {
    val ast = parse("::label::")
    val stat = ast.block.stats.head.asInstanceOf[LabelStat]
    assert(stat.name == "label")
  }

  // ============================================================================
  // VARARG
  // ============================================================================

  test("vararg: in expression") {
    val ast = parse("function f(...) return ... end")
    val stat = ast.block.stats.head.asInstanceOf[FunctionStat]
    val ret = stat.body.block.retstat.get
    assert(ret.explist.head.isInstanceOf[VarargExp])
  }

  // ============================================================================
  // FILE TESTS (regression)
  // ============================================================================

  test("file: test_if.lua") {
    val ast = parseFile("lua_programs/test_if.lua")
    assert(ast.block.stats.length == 2)
    val assign = ast.block.stats(0).asInstanceOf[AssignStat]
    assert(assign.varlist.head.asInstanceOf[NameVar].name == "x")
    val ifstat = ast.block.stats(1).asInstanceOf[IfStat]
    assert(ifstat.condition.asInstanceOf[BinopExp].op == ">")
  }

  test("file: test_while.lua") {
    val ast = parseFile("lua_programs/test_while.lua")
    val assign = ast.block.stats(0).asInstanceOf[AssignStat]
    assert(assign.varlist.head.asInstanceOf[NameVar].name == "i")
    val whilestat = ast.block.stats(1).asInstanceOf[WhileStat]
    assert(whilestat.condition.asInstanceOf[BinopExp].op == "<")
  }

  test("file: test_function.lua") {
    val ast = parseFile("lua_programs/test_function.lua")
    val func = ast.block.stats(0).asInstanceOf[FunctionStat]
    assert(func.name.names.head == "add")
    assert(func.body.params.length == 2)
  }

  test("file: test_table.lua") {
    val ast = parseFile("lua_programs/test_table.lua")
    val assign = ast.block.stats(0).asInstanceOf[AssignStat]
    assert(assign.varlist.head.asInstanceOf[NameVar].name == "person")
    val table = assign.explist.head.asInstanceOf[TableConstructor]
    assert(table.fields.length == 2)
  }

  test("file: test_for.lua") {
    val ast = parseFile("lua_programs/test_for.lua")
    val fornum = ast.block.stats(0).asInstanceOf[ForNumStat]
    assert(fornum.name == "i")
    val forin = ast.block.stats(1).asInstanceOf[ForInStat]
    assert(forin.names.length == 2)
  }
}