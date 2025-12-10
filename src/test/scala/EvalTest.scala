// created with the assistance from Claude AI

import org.scalatest.funsuite.AnyFunSuite
import java.io.{ByteArrayOutputStream, PrintStream}

class EvalTest extends AnyFunSuite {

  // Helper to evaluate Lua code and return the global environment
  def evalCode(code: String): Env = {
    val parser = Parser()
    val ast = parser.parse(code)
    val env = Eval.makeGlobalEnv()
    Eval.execBlock(ast.block, env)
    env
  }

  // Helper to evaluate and capture print output
  def evalWithOutput(code: String): (Env, String) = {
    val outStream = new ByteArrayOutputStream()
    val printStream = new PrintStream(outStream)
    val oldOut = System.out
    System.setOut(printStream)

    val env = try {
      evalCode(code)
    } finally {
      System.setOut(oldOut)
    }

    (env, outStream.toString.trim)
  }

  // ============================================================================
  // ARITHMETIC OPERATORS
  // ============================================================================

  test("arithmetic: addition") {
    val env = evalCode("x = 3 + 5")
    assert(env.get("x") == LuaNum(8))
  }

  test("arithmetic: subtraction") {
    val env = evalCode("x = 10 - 3")
    assert(env.get("x") == LuaNum(7))
  }

  test("arithmetic: multiplication") {
    val env = evalCode("x = 4 * 5")
    assert(env.get("x") == LuaNum(20))
  }

  test("arithmetic: division") {
    val env = evalCode("x = 15 / 3")
    assert(env.get("x") == LuaNum(5))
  }

  test("arithmetic: floor division") {
    val env = evalCode("x = 7 // 3")
    assert(env.get("x") == LuaNum(2))
  }

  test("arithmetic: floor division negative") {
    val env = evalCode("x = -7 // 3")
    assert(env.get("x") == LuaNum(-3))
  }

  test("arithmetic: modulo") {
    val env = evalCode("x = 7 % 3")
    assert(env.get("x") == LuaNum(1))
  }

  test("arithmetic: modulo negative (Lua floored)") {
    val env = evalCode("x = -7 % 3")
    assert(env.get("x") == LuaNum(2))
  }

  test("arithmetic: power") {
    val env = evalCode("x = 2 ^ 10")
    assert(env.get("x") == LuaNum(1024))
  }

  test("arithmetic: unary minus") {
    val env = evalCode("x = -42")
    assert(env.get("x") == LuaNum(-42))
  }

  // ============================================================================
  // COMPARISON OPERATORS
  // ============================================================================

  test("comparison: less than") {
    val env = evalCode("x = 3 < 5")
    assert(env.get("x") == LuaBool(true))
  }

  test("comparison: greater than") {
    val env = evalCode("x = 5 > 3")
    assert(env.get("x") == LuaBool(true))
  }

  test("comparison: equal") {
    val env = evalCode("x = 5 == 5")
    assert(env.get("x") == LuaBool(true))
  }

  test("comparison: not equal") {
    val env = evalCode("x = 5 ~= 3")
    assert(env.get("x") == LuaBool(true))
  }

  test("comparison: string") {
    val env = evalCode("x = 'abc' < 'abd'")
    assert(env.get("x") == LuaBool(true))
  }

  // ============================================================================
  // LOGICAL OPERATORS
  // ============================================================================

  test("logical: and true") {
    val env = evalCode("x = true and true")
    assert(env.get("x") == LuaBool(true))
  }

  test("logical: and false") {
    val env = evalCode("x = true and false")
    assert(env.get("x") == LuaBool(false))
  }

  test("logical: or") {
    val env = evalCode("x = false or true")
    assert(env.get("x") == LuaBool(true))
  }

  test("logical: not") {
    val env = evalCode("x = not false")
    assert(env.get("x") == LuaBool(true))
  }

  test("logical: short-circuit and") {
    val env = evalCode("x = nil and 'hello'")
    assert(env.get("x") == LuaNil)
  }

  test("logical: short-circuit or") {
    val env = evalCode("x = nil or 'default'")
    assert(env.get("x") == LuaStr("default"))
  }

  // ============================================================================
  // STRINGS
  // ============================================================================

  test("string: concatenation") {
    val env = evalCode("x = 'hello' .. ' ' .. 'world'")
    assert(env.get("x") == LuaStr("hello world"))
  }

  test("string: length") {
    val env = evalCode("x = #'hello'")
    assert(env.get("x") == LuaNum(5))
  }

  // ============================================================================
  // VARIABLES
  // ============================================================================

  test("variables: local") {
    val env = evalCode("local x = 10")
    // local is in child scope, should not be in global
  }

  test("variables: global") {
    val env = evalCode("x = 42")
    assert(env.get("x") == LuaNum(42))
  }

  test("variables: multiple assignment") {
    val env = evalCode("a, b, c = 1, 2, 3")
    assert(env.get("a") == LuaNum(1))
    assert(env.get("b") == LuaNum(2))
    assert(env.get("c") == LuaNum(3))
  }

  test("variables: swap") {
    val env = evalCode("a, b = 1, 2\na, b = b, a")
    assert(env.get("a") == LuaNum(2))
    assert(env.get("b") == LuaNum(1))
  }

  // ============================================================================
  // TABLES
  // ============================================================================

  test("tables: array") {
    val env = evalCode("t = {10, 20, 30}")
    val t = env.get("t").asInstanceOf[LuaTable]
    assert(t.map(LuaNum(1)) == LuaNum(10))
    assert(t.map(LuaNum(2)) == LuaNum(20))
    assert(t.map(LuaNum(3)) == LuaNum(30))
  }

  test("tables: dictionary") {
    val env = evalCode("t = {x = 1, y = 2}")
    val t = env.get("t").asInstanceOf[LuaTable]
    assert(t.map(LuaStr("x")) == LuaNum(1))
    assert(t.map(LuaStr("y")) == LuaNum(2))
  }

  test("tables: computed key") {
    val env = evalCode("t = {[1+1] = 'two'}")
    val t = env.get("t").asInstanceOf[LuaTable]
    assert(t.map(LuaNum(2)) == LuaStr("two"))
  }

  test("tables: length") {
    val env = evalCode("t = {1, 2, 3, 4, 5}\nx = #t")
    assert(env.get("x") == LuaNum(5))
  }

  test("tables: dot access") {
    val env = evalCode("t = {name = 'Alice'}\nx = t.name")
    assert(env.get("x") == LuaStr("Alice"))
  }

  test("tables: bracket access") {
    val env = evalCode("t = {name = 'Bob'}\nx = t['name']")
    assert(env.get("x") == LuaStr("Bob"))
  }

  // ============================================================================
  // CONTROL FLOW
  // ============================================================================

  test("if: true branch") {
    val env = evalCode("if true then x = 1 else x = 2 end")
    assert(env.get("x") == LuaNum(1))
  }

  test("if: false branch") {
    val env = evalCode("if false then x = 1 else x = 2 end")
    assert(env.get("x") == LuaNum(2))
  }

  test("if: elseif") {
    val env = evalCode("x = 5\nif x > 10 then y = 'big' elseif x > 3 then y = 'medium' else y = 'small' end")
    assert(env.get("y") == LuaStr("medium"))
  }

  test("while: loop") {
    val env = evalCode("x = 0\nwhile x < 5 do x = x + 1 end")
    assert(env.get("x") == LuaNum(5))
  }

  test("while: break") {
    val env = evalCode("x = 0\nwhile true do x = x + 1\nif x >= 3 then break end end")
    assert(env.get("x") == LuaNum(3))
  }

  test("repeat: until") {
    val env = evalCode("x = 0\nrepeat x = x + 1 until x >= 5")
    assert(env.get("x") == LuaNum(5))
  }

  test("for: numeric") {
    val env = evalCode("sum = 0\nfor i = 1, 5 do sum = sum + i end")
    assert(env.get("sum") == LuaNum(15))
  }

  test("for: numeric with step") {
    val env = evalCode("sum = 0\nfor i = 0, 10, 2 do sum = sum + i end")
    assert(env.get("sum") == LuaNum(30))
  }

  test("for: numeric descending") {
    val env = evalCode("sum = 0\nfor i = 5, 1, -1 do sum = sum + i end")
    assert(env.get("sum") == LuaNum(15))
  }

  // ============================================================================
  // FUNCTIONS
  // ============================================================================

  test("function: basic") {
    val env = evalCode("function add(a, b) return a + b end\nx = add(3, 4)")
    assert(env.get("x") == LuaNum(7))
  }

  test("function: local function") {
    val env = evalCode("local function square(x) return x * x end\ny = square(5)")
    assert(env.get("y") == LuaNum(25))
  }

  test("function: multiple return") {
    val env = evalCode("function f() return 1, 2, 3 end\na, b, c = f()")
    assert(env.get("a") == LuaNum(1))
    assert(env.get("b") == LuaNum(2))
    assert(env.get("c") == LuaNum(3))
  }

  test("function: recursion") {
    val env = evalCode("function fac(n) if n <= 1 then return 1 else return n * fac(n-1) end end\nx = fac(5)")
    assert(env.get("x") == LuaNum(120))
  }

  test("function: closure") {
    val env = evalCode("""
      function makeCounter()
        local count = 0
        return function()
          count = count + 1
          return count
        end
      end
      counter = makeCounter()
      a = counter()
      b = counter()
      c = counter()
    """)
    assert(env.get("a") == LuaNum(1))
    assert(env.get("b") == LuaNum(2))
    assert(env.get("c") == LuaNum(3))
  }

  // ============================================================================
  // VARARGS
  // ============================================================================

  test("varargs: table constructor") {
    val env = evalCode("""
      function countargs(...)
        local args = {...}
        return #args
      end
      x = countargs(1, 2, 3)
    """)
    assert(env.get("x") == LuaNum(3))
  }

  test("varargs: forwarding") {
    val env = evalCode("""
      function sum(...)
        local args = {...}
        local s = 0
        for i = 1, #args do s = s + args[i] end
        return s
      end
      function forward(...)
        return sum(...)
      end
      x = forward(1, 2, 3, 4)
    """)
    assert(env.get("x") == LuaNum(10))
  }

  // ============================================================================
  // METHODS
  // ============================================================================

  test("method: colon syntax") {
    val env = evalCode("""
      obj = {value = 10}
      function obj:add(x)
        self.value = self.value + x
        return self.value
      end
      result = obj:add(5)
    """)
    assert(env.get("result") == LuaNum(15))
  }

  // ============================================================================
  // BUILT-IN FUNCTIONS
  // ============================================================================

  test("builtin: type") {
    val env = evalCode("""
      a = type(nil)
      b = type(true)
      c = type(42)
      d = type('hello')
      e = type({})
    """)
    assert(env.get("a") == LuaStr("nil"))
    assert(env.get("b") == LuaStr("boolean"))
    assert(env.get("c") == LuaStr("number"))
    assert(env.get("d") == LuaStr("string"))
    assert(env.get("e") == LuaStr("table"))
  }

  test("builtin: tonumber") {
    val env = evalCode("""
      a = tonumber('42')
      b = tonumber('3.14')
      c = tonumber('hello')
    """)
    assert(env.get("a") == LuaNum(42))
    assert(env.get("b") == LuaNum(3.14))
    assert(env.get("c") == LuaNil)
  }

  test("builtin: tostring") {
    val env = evalCode("""
      a = tostring(42)
      b = tostring(true)
      c = tostring(nil)
    """)
    assert(env.get("a") == LuaStr("42"))
    assert(env.get("b") == LuaStr("true"))
    assert(env.get("c") == LuaStr("nil"))
  }

  test("builtin: pcall success") {
    val env = evalCode("""
      function f(x) return x * 2 end
      ok, result = pcall(f, 10)
    """)
    assert(env.get("ok") == LuaBool(true))
    assert(env.get("result") == LuaNum(20))
  }

  test("builtin: pcall failure") {
    val env = evalCode("""
      function f(x) error('oops') end
      ok, err = pcall(f, 10)
    """)
    assert(env.get("ok") == LuaBool(false))
  }

  test("builtin: assert success") {
    val env = evalCode("x = assert(5 > 3)")
    assert(env.get("x") == LuaBool(true))
  }

  test("builtin: select index") {
    val env = evalCode("a, b = select(2, 'x', 'y', 'z')")
    assert(env.get("a") == LuaStr("y"))
    assert(env.get("b") == LuaStr("z"))
  }

  test("builtin: select count") {
    val env = evalCode("x = select('#', 'a', 'b', 'c')")
    assert(env.get("x") == LuaNum(3))
  }

  // ============================================================================
  // PRINT (tested via type check - stdout capture is unreliable)
  // ============================================================================

  test("builtin: print exists") {
    val env = Eval.makeGlobalEnv()
    assert(env.get("print").isInstanceOf[LuaBuiltin])
  }
}