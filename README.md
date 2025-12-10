# LUA Interpreter

A Lua 5.3 interpreter written in Scala for educational purposes.

## Features

- Tokenizer (lexical analysis)
- Recursive descent parser
- AST generation with JSON export
- Tree-walking evaluator
- Core Lua semantics (closures, multiple returns, varargs)

## Supported

- Data types: nil, boolean, number, string, table, function
- Control flow: if/elseif/else, while, repeat, for (numeric & generic)
- Operators: arithmetic, comparison, logical, bitwise, concatenation
- Functions: definitions, closures, varargs, multiple returns
- Tables: array and dictionary style, dot and bracket access
- Built-ins: print, type, pairs, ipairs, pcall, assert, etc.

## Setup

Requires Scala 3 and sbt.

```bash
sbt compile
sbt run lua_programs/add2.lua
```

## Run Tests

```bash
sbt test
```

## Project Structure

```
src/main/scala/
  Tokenizer.scala   - Lexical analysis
  Parser.scala      - Recursive descent parser
  AST.scala         - AST node definitions
  Eval.scala        - Tree-walking evaluator
  Main.scala        - Entry point

src/test/scala/     - Unit and regression tests
lua_programs/       - Example Lua programs
```

## Not Implemented

- Standard library modules (string.*, table.*, math.*, io.*)
- Metatables and metamethods
- Coroutines
- Long strings, hex numbers