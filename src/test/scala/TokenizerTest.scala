// created with the assistance from Claude AI

import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source

class TokenizerTest extends AnyFunSuite {

  def getTokensFromFile(path: String): List[Token] = {
    val source = Source.fromFile(path)
    val code = source.mkString
    source.close()

    val tokenizer = Tokenizer(code)
    var tokens = List[Token]()
    var token = tokenizer.getNextToken()
    while (token != null) {
      tokens = tokens :+ token
      token = tokenizer.getNextToken()
    }
    tokens
  }

  // x = 10
  // if x > 5 then
  //     y = "big"
  // else
  //     y = "small"
  // end
  test("test_if.lua") {
    val tokens = getTokensFromFile("lua_programs/test_if.lua")

    assert(tokens(0).kind == "NAME")
    assert(tokens(1).kind == "ASSIGN")
    assert(tokens(2).kind == "NUMBER")
    assert(tokens(3).kind == "IF")
    assert(tokens(4).kind == "NAME")
    assert(tokens(5).kind == "GT")
    assert(tokens(6).kind == "NUMBER")
    assert(tokens(7).kind == "THEN")
    assert(tokens(8).kind == "NAME")
    assert(tokens(9).kind == "ASSIGN")
    assert(tokens(10).kind == "STRING")
    assert(tokens(11).kind == "ELSE")
    assert(tokens(12).kind == "NAME")
    assert(tokens(13).kind == "ASSIGN")
    assert(tokens(14).kind == "STRING")
    assert(tokens(15).kind == "END")
  }

  // i = 0
  // while i < 10 do
  //     i = i + 1
  // end
  test("test_while.lua") {
    val tokens = getTokensFromFile("lua_programs/test_while.lua")

    assert(tokens(0).kind == "NAME")
    assert(tokens(1).kind == "ASSIGN")
    assert(tokens(2).kind == "NUMBER")
    assert(tokens(3).kind == "WHILE")
    assert(tokens(4).kind == "NAME")
    assert(tokens(5).kind == "LT")
    assert(tokens(6).kind == "NUMBER")
    assert(tokens(7).kind == "DO")
    assert(tokens(8).kind == "NAME")
    assert(tokens(9).kind == "ASSIGN")
    assert(tokens(10).kind == "NAME")
    assert(tokens(11).kind == "PLUS")
    assert(tokens(12).kind == "NUMBER")
    assert(tokens(13).kind == "END")
  }

  // function add(a, b)
  //     return a + b
  // end
  // result = add(3, 5)
  test("test_function.lua") {
    val tokens = getTokensFromFile("lua_programs/test_function.lua")

    assert(tokens(0).kind == "FUNCTION")
    assert(tokens(1).kind == "NAME")
    assert(tokens(2).kind == "LPAREN")
    assert(tokens(3).kind == "NAME")
    assert(tokens(4).kind == "COMMA")
    assert(tokens(5).kind == "NAME")
    assert(tokens(6).kind == "RPAREN")
    assert(tokens(7).kind == "RETURN")
    assert(tokens(8).kind == "NAME")
    assert(tokens(9).kind == "PLUS")
    assert(tokens(10).kind == "NAME")
    assert(tokens(11).kind == "END")
    assert(tokens(12).kind == "NAME")
    assert(tokens(13).kind == "ASSIGN")
    assert(tokens(14).kind == "NAME")
    assert(tokens(15).kind == "LPAREN")
    assert(tokens(16).kind == "NUMBER")
    assert(tokens(17).kind == "COMMA")
    assert(tokens(18).kind == "NUMBER")
    assert(tokens(19).kind == "RPAREN")
  }

  // person = {
  //     name = "Alice",
  //     age = 30
  // }
  // x = person.name
  // y = person["age"]
  test("test_table.lua") {
    val tokens = getTokensFromFile("lua_programs/test_table.lua")

    assert(tokens(0).kind == "NAME")
    assert(tokens(1).kind == "ASSIGN")
    assert(tokens(2).kind == "LBRACE")
    assert(tokens(3).kind == "NAME")
    assert(tokens(4).kind == "ASSIGN")
    assert(tokens(5).kind == "STRING")
    assert(tokens(6).kind == "COMMA")
    assert(tokens(7).kind == "NAME")
    assert(tokens(8).kind == "ASSIGN")
    assert(tokens(9).kind == "NUMBER")
    assert(tokens(10).kind == "RBRACE")
    assert(tokens(11).kind == "NAME")
    assert(tokens(12).kind == "ASSIGN")
    assert(tokens(13).kind == "NAME")
    assert(tokens(14).kind == "DOT")
    assert(tokens(15).kind == "NAME")
    assert(tokens(16).kind == "NAME")
    assert(tokens(17).kind == "ASSIGN")
    assert(tokens(18).kind == "NAME")
    assert(tokens(19).kind == "LBRACKET")
    assert(tokens(20).kind == "STRING")
    assert(tokens(21).kind == "RBRACKET")
  }

  // for i = 1, 10, 1 do
  //     x = i * 2
  // end
  // for k, v in pairs(t) do
  //     print(k, v)
  // end
  test("test_for.lua") {
    val tokens = getTokensFromFile("lua_programs/test_for.lua")

    assert(tokens(0).kind == "FOR")
    assert(tokens(1).kind == "NAME")
    assert(tokens(2).kind == "ASSIGN")
    assert(tokens(3).kind == "NUMBER")
    assert(tokens(4).kind == "COMMA")
    assert(tokens(5).kind == "NUMBER")
    assert(tokens(6).kind == "COMMA")
    assert(tokens(7).kind == "NUMBER")
    assert(tokens(8).kind == "DO")
    assert(tokens(9).kind == "NAME")
    assert(tokens(10).kind == "ASSIGN")
    assert(tokens(11).kind == "NAME")
    assert(tokens(12).kind == "STAR")
    assert(tokens(13).kind == "NUMBER")
    assert(tokens(14).kind == "END")
    assert(tokens(15).kind == "FOR")
    assert(tokens(16).kind == "NAME")
    assert(tokens(17).kind == "COMMA")
    assert(tokens(18).kind == "NAME")
    assert(tokens(19).kind == "IN")
    assert(tokens(20).kind == "NAME")
    assert(tokens(21).kind == "LPAREN")
    assert(tokens(22).kind == "NAME")
    assert(tokens(23).kind == "RPAREN")
    assert(tokens(24).kind == "DO")
    assert(tokens(25).kind == "NAME")
    assert(tokens(26).kind == "LPAREN")
    assert(tokens(27).kind == "NAME")
    assert(tokens(28).kind == "COMMA")
    assert(tokens(29).kind == "NAME")
    assert(tokens(30).kind == "RPAREN")
    assert(tokens(31).kind == "END")
  }

}