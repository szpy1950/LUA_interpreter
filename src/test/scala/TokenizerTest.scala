// created with the assistance from Claude AI

import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source

class TokenizerTest extends AnyFunSuite {

  // helper to get all tokens from code string
  def tokenize(code: String): List[Token] = {
    val tokenizer = Tokenizer(code)
    var tokens = List[Token]()
    var token = tokenizer.getNextToken()
    while (token != null) {
      tokens = tokens :+ token
      token = tokenizer.getNextToken()
    }
    tokens
  }

  // helper to get tokens from file
  def tokenizeFile(path: String): List[Token] = {
    val source = Source.fromFile(path)
    val code = source.mkString
    source.close()
    tokenize(code)
  }

  // ============================================================================
  // KEYWORDS
  // ============================================================================

  test("keyword: and") {
    val tokens = tokenize("and")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "AND")
  }

  test("keyword: break") {
    val tokens = tokenize("break")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "BREAK")
  }

  test("keyword: do") {
    val tokens = tokenize("do")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "DO")
  }

  test("keyword: else") {
    val tokens = tokenize("else")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "ELSE")
  }

  test("keyword: elseif") {
    val tokens = tokenize("elseif")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "ELSEIF")
  }

  test("keyword: end") {
    val tokens = tokenize("end")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "END")
  }

  test("keyword: false") {
    val tokens = tokenize("false")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "FALSE")
  }

  test("keyword: for") {
    val tokens = tokenize("for")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "FOR")
  }

  test("keyword: function") {
    val tokens = tokenize("function")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "FUNCTION")
  }

  test("keyword: goto") {
    val tokens = tokenize("goto")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "GOTO")
  }

  test("keyword: if") {
    val tokens = tokenize("if")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "IF")
  }

  test("keyword: in") {
    val tokens = tokenize("in")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "IN")
  }

  test("keyword: local") {
    val tokens = tokenize("local")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "LOCAL")
  }

  test("keyword: nil") {
    val tokens = tokenize("nil")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NIL")
  }

  test("keyword: not") {
    val tokens = tokenize("not")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NOT")
  }

  test("keyword: or") {
    val tokens = tokenize("or")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "OR")
  }

  test("keyword: repeat") {
    val tokens = tokenize("repeat")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "REPEAT")
  }

  test("keyword: return") {
    val tokens = tokenize("return")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "RETURN")
  }

  test("keyword: then") {
    val tokens = tokenize("then")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "THEN")
  }

  test("keyword: true") {
    val tokens = tokenize("true")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "TRUE")
  }

  test("keyword: until") {
    val tokens = tokenize("until")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "UNTIL")
  }

  test("keyword: while") {
    val tokens = tokenize("while")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "WHILE")
  }

  // ============================================================================
  // NUMBERS
  // ============================================================================

  test("number: integer") {
    val tokens = tokenize("42")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NUMBER")
    assert(tokens(0).value == "42")
  }

  test("number: zero") {
    val tokens = tokenize("0")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NUMBER")
    assert(tokens(0).value == "0")
  }

  test("number: float") {
    val tokens = tokenize("3.14")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NUMBER")
    assert(tokens(0).value == "3.14")
  }

  test("number: float starting with zero") {
    val tokens = tokenize("0.5")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NUMBER")
    assert(tokens(0).value == "0.5")
  }

  test("number: large integer") {
    val tokens = tokenize("1234567890")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NUMBER")
    assert(tokens(0).value == "1234567890")
  }

  // ============================================================================
  // STRINGS
  // ============================================================================

  test("string: double quotes") {
    val tokens = tokenize("\"hello\"")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "STRING")
    assert(tokens(0).value == "hello")
  }

  test("string: single quotes") {
    val tokens = tokenize("'world'")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "STRING")
    assert(tokens(0).value == "world")
  }

  test("string: empty double quotes") {
    val tokens = tokenize("\"\"")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "STRING")
    assert(tokens(0).value == "")
  }

  test("string: empty single quotes") {
    val tokens = tokenize("''")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "STRING")
    assert(tokens(0).value == "")
  }

  test("string: with spaces") {
    val tokens = tokenize("\"hello world\"")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "STRING")
    assert(tokens(0).value == "hello world")
  }

  // ============================================================================
  // OPERATORS - ARITHMETIC
  // ============================================================================

  test("operator: plus") {
    val tokens = tokenize("+")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "PLUS")
  }

  test("operator: minus") {
    val tokens = tokenize("-")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "MINUS")
  }

  test("operator: star") {
    val tokens = tokenize("*")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "STAR")
  }

  test("operator: slash") {
    val tokens = tokenize("/")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "SLASH")
  }

  test("operator: floor div") {
    val tokens = tokenize("//")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "FLOORDIV")
  }

  test("operator: percent") {
    val tokens = tokenize("%")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "PERCENT")
  }

  test("operator: caret (power)") {
    val tokens = tokenize("^")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "CARET")
  }

  // ============================================================================
  // OPERATORS - COMPARISON
  // ============================================================================

  test("operator: less than") {
    val tokens = tokenize("<")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "LT")
  }

  test("operator: greater than") {
    val tokens = tokenize(">")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "GT")
  }

  test("operator: less or equal") {
    val tokens = tokenize("<=")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "LE")
  }

  test("operator: greater or equal") {
    val tokens = tokenize(">=")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "GE")
  }

  test("operator: equal") {
    val tokens = tokenize("==")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "EQ")
  }

  test("operator: not equal") {
    val tokens = tokenize("~=")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NE")
  }

  // ============================================================================
  // OPERATORS - BITWISE
  // ============================================================================

  test("operator: ampersand") {
    val tokens = tokenize("&")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "AMPERSAND")
  }

  test("operator: pipe") {
    val tokens = tokenize("|")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "PIPE")
  }

  test("operator: tilde") {
    val tokens = tokenize("~")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "TILDE")
  }

  test("operator: shift left") {
    val tokens = tokenize("<<")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "SHL")
  }

  test("operator: shift right") {
    val tokens = tokenize(">>")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "SHR")
  }

  // ============================================================================
  // OPERATORS - OTHER
  // ============================================================================

  test("operator: hash (length)") {
    val tokens = tokenize("#")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "HASH")
  }

  test("operator: concat") {
    val tokens = tokenize("..")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "DOTDOT")
  }

  test("operator: vararg") {
    val tokens = tokenize("...")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "DOTDOTDOT")
  }

  test("operator: assign") {
    val tokens = tokenize("=")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "ASSIGN")
  }

  // ============================================================================
  // DELIMITERS
  // ============================================================================

  test("delimiter: lparen") {
    val tokens = tokenize("(")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "LPAREN")
  }

  test("delimiter: rparen") {
    val tokens = tokenize(")")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "RPAREN")
  }

  test("delimiter: lbracket") {
    val tokens = tokenize("[")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "LBRACKET")
  }

  test("delimiter: rbracket") {
    val tokens = tokenize("]")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "RBRACKET")
  }

  test("delimiter: lbrace") {
    val tokens = tokenize("{")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "LBRACE")
  }

  test("delimiter: rbrace") {
    val tokens = tokenize("}")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "RBRACE")
  }

  test("delimiter: comma") {
    val tokens = tokenize(",")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "COMMA")
  }

  test("delimiter: semicolon") {
    val tokens = tokenize(";")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "SEMICOLON")
  }

  test("delimiter: colon") {
    val tokens = tokenize(":")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "COLON")
  }

  test("delimiter: double colon") {
    val tokens = tokenize("::")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "DOUBLECOLON")
  }

  test("delimiter: dot") {
    val tokens = tokenize(".")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "DOT")
  }

  // ============================================================================
  // NAMES (IDENTIFIERS)
  // ============================================================================

  test("name: simple") {
    val tokens = tokenize("foo")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NAME")
    assert(tokens(0).value == "foo")
  }

  test("name: with underscore") {
    val tokens = tokenize("my_var")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NAME")
    assert(tokens(0).value == "my_var")
  }

  test("name: starting with underscore") {
    val tokens = tokenize("_private")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NAME")
    assert(tokens(0).value == "_private")
  }

  test("name: with numbers") {
    val tokens = tokenize("var123")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NAME")
    assert(tokens(0).value == "var123")
  }

  test("name: uppercase") {
    val tokens = tokenize("CONSTANT")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NAME")
    assert(tokens(0).value == "CONSTANT")
  }

  test("name: mixed case") {
    val tokens = tokenize("myVariable")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NAME")
    assert(tokens(0).value == "myVariable")
  }

  // ============================================================================
  // WHITESPACE AND COMMENTS
  // ============================================================================

  test("whitespace: skipped") {
    val tokens = tokenize("   x   ")
    assert(tokens.length == 1)
    assert(tokens(0).kind == "NAME")
    assert(tokens(0).value == "x")
  }

  test("whitespace: newlines skipped") {
    val tokens = tokenize("x\n\ny")
    assert(tokens.length == 2)
    assert(tokens(0).value == "x")
    assert(tokens(1).value == "y")
  }

  test("whitespace: tabs skipped") {
    val tokens = tokenize("x\t\ty")
    assert(tokens.length == 2)
  }

  test("comment: single line skipped") {
    val tokens = tokenize("x -- this is a comment\ny")
    assert(tokens.length == 2)
    assert(tokens(0).value == "x")
    assert(tokens(1).value == "y")
  }

  test("comment: multi line skipped") {
    val tokens = tokenize("x --[[multi\nline\ncomment]]y")
    assert(tokens.length == 2)
    assert(tokens(0).value == "x")
    assert(tokens(1).value == "y")
  }

  // ============================================================================
  // COMBINED TOKENS
  // ============================================================================

  test("combined: simple assignment") {
    val tokens = tokenize("x = 10")
    assert(tokens.length == 3)
    assert(tokens(0).kind == "NAME")
    assert(tokens(1).kind == "ASSIGN")
    assert(tokens(2).kind == "NUMBER")
  }

  test("combined: arithmetic expression") {
    val tokens = tokenize("1 + 2 * 3")
    assert(tokens.length == 5)
    assert(tokens(0).kind == "NUMBER")
    assert(tokens(1).kind == "PLUS")
    assert(tokens(2).kind == "NUMBER")
    assert(tokens(3).kind == "STAR")
    assert(tokens(4).kind == "NUMBER")
  }

  test("combined: function call") {
    val tokens = tokenize("print(x)")
    assert(tokens.length == 4)
    assert(tokens(0).kind == "NAME")
    assert(tokens(1).kind == "LPAREN")
    assert(tokens(2).kind == "NAME")
    assert(tokens(3).kind == "RPAREN")
  }

  test("combined: if statement tokens") {
    val tokens = tokenize("if x > 5 then y = 1 end")
    assert(tokens(0).kind == "IF")
    assert(tokens(1).kind == "NAME")
    assert(tokens(2).kind == "GT")
    assert(tokens(3).kind == "NUMBER")
    assert(tokens(4).kind == "THEN")
    assert(tokens(5).kind == "NAME")
    assert(tokens(6).kind == "ASSIGN")
    assert(tokens(7).kind == "NUMBER")
    assert(tokens(8).kind == "END")
  }

  test("combined: while loop tokens") {
    val tokens = tokenize("while i < 10 do i = i + 1 end")
    assert(tokens(0).kind == "WHILE")
    assert(tokens(1).kind == "NAME")
    assert(tokens(2).kind == "LT")
    assert(tokens(3).kind == "NUMBER")
    assert(tokens(4).kind == "DO")
  }

  test("combined: for loop tokens") {
    val tokens = tokenize("for i = 1, 10 do end")
    assert(tokens(0).kind == "FOR")
    assert(tokens(1).kind == "NAME")
    assert(tokens(2).kind == "ASSIGN")
    assert(tokens(3).kind == "NUMBER")
    assert(tokens(4).kind == "COMMA")
    assert(tokens(5).kind == "NUMBER")
    assert(tokens(6).kind == "DO")
    assert(tokens(7).kind == "END")
  }

  test("combined: table constructor") {
    val tokens = tokenize("{x = 1, y = 2}")
    assert(tokens(0).kind == "LBRACE")
    assert(tokens(1).kind == "NAME")
    assert(tokens(2).kind == "ASSIGN")
    assert(tokens(3).kind == "NUMBER")
    assert(tokens(4).kind == "COMMA")
    assert(tokens(5).kind == "NAME")
    assert(tokens(6).kind == "ASSIGN")
    assert(tokens(7).kind == "NUMBER")
    assert(tokens(8).kind == "RBRACE")
  }

  test("combined: function definition") {
    val tokens = tokenize("function foo(a, b) return a + b end")
    assert(tokens(0).kind == "FUNCTION")
    assert(tokens(1).kind == "NAME")
    assert(tokens(2).kind == "LPAREN")
    assert(tokens(3).kind == "NAME")
    assert(tokens(4).kind == "COMMA")
    assert(tokens(5).kind == "NAME")
    assert(tokens(6).kind == "RPAREN")
    assert(tokens(7).kind == "RETURN")
  }

  test("combined: method call") {
    val tokens = tokenize("obj:method()")
    assert(tokens(0).kind == "NAME")
    assert(tokens(1).kind == "COLON")
    assert(tokens(2).kind == "NAME")
    assert(tokens(3).kind == "LPAREN")
    assert(tokens(4).kind == "RPAREN")
  }

  // ============================================================================
  // FILE TESTS (regression)
  // ============================================================================

  test("file: test_if.lua") {
    val tokens = tokenizeFile("lua_programs/test_if.lua")
    assert(tokens(0).kind == "NAME")
    assert(tokens(1).kind == "ASSIGN")
    assert(tokens(2).kind == "NUMBER")
    assert(tokens(3).kind == "IF")
  }

  test("file: test_while.lua") {
    val tokens = tokenizeFile("lua_programs/test_while.lua")
    assert(tokens(0).kind == "NAME")
    assert(tokens(3).kind == "WHILE")
  }

  test("file: test_function.lua") {
    val tokens = tokenizeFile("lua_programs/test_function.lua")
    assert(tokens(0).kind == "FUNCTION")
  }

  test("file: test_table.lua") {
    val tokens = tokenizeFile("lua_programs/test_table.lua")
    assert(tokens(0).kind == "NAME")
    assert(tokens(2).kind == "LBRACE")
  }

  test("file: test_for.lua") {
    val tokens = tokenizeFile("lua_programs/test_for.lua")
    assert(tokens(0).kind == "FOR")
  }
}