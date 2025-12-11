// parts created and verified with the assistance from Claude AI

import scala.collection.mutable.ListBuffer

/**
 * Recursive descent parser in LUA
 * It uses a single token lookahead ll(1) to decide what gramamr rule we will apply.
 * The tokenizer converts source code into an Abstract Syntax Tree
 */

class Parser() {
  var tokenizer: Tokenizer = null
  var lookahead: Token = null

  /**
   * Initializes the tokenizer and starts parsing
   */
  def parse(code: String): Chunk = {
    tokenizer = Tokenizer(code)
    lookahead = tokenizer.getNextToken()
    parseChunk()
  }

  /**
   * Every LUA code is a chunk. This is the main entry point.
   * from LUA BNF:
   * chunk ::= block
   * */
  def parseChunk(): Chunk = {
    Chunk(parseBlock())
  }


  /**
   * from LUA BNF:
   * block ::= {stat} [retstat]
   */
  def parseBlock(): Block = {
    // println("in parseBlock")
    val stats = ListBuffer[Stat]()
    while (lookahead != null && !isBlockEnd() && lookahead.kind != "RETURN") {
      val stat = parseStat()
      if (stat != null) {
        stats += stat
      }
    }
    val retstat = parseRetstat()
    Block(stats.toList, retstat)
  }

  def parseRetstat(): Option[RetStat] = {
    // println("retstat")
    if (lookahead != null && lookahead.kind == "RETURN") {
      eat("RETURN")
      val exps = if (lookahead != null && !isBlockEnd() && lookahead.kind != "SEMICOLON") {
        parseExpList()
      } else List()
      if (lookahead != null && lookahead.kind == "SEMICOLON") {
        eat("SEMICOLON")
      }
      Some(RetStat(exps))
    } else None
  }

  // check if we hit end of block
  def isBlockEnd(): Boolean = {
    if (lookahead == null) return true
    lookahead.kind match {
      case "END" | "ELSE" | "ELSEIF" | "UNTIL" => true
      case _ => false
    }
  }

  def isKeyword(s: String): Boolean = {
    val keywords = Set("and", "break", "do", "else", "elseif", "end", "false",
      "for", "function", "goto", "if", "in", "local", "nil", "not", "or",
      "repeat", "return", "then", "true", "until", "while")
    keywords.contains(s)
  }

  def parseStat(): Stat = {
    if (lookahead == null) return null
    // println(s"parseStat: ${lookahead.kind}")

    lookahead.kind match {
      case "SEMICOLON" =>
        eat("SEMICOLON")
        parseStat()

      case "NAME" | "LPAREN" =>
        parseAssignOrCall()

      case "IF" =>
        parseIf()

      case "WHILE" =>
        parseWhile()

      case "FOR" =>
        parseFor()

      case "FUNCTION" =>
        parseFunction()

      case "LOCAL" =>
        parseLocal()

      case "DO" =>
        parseDo()

      case "REPEAT" =>
        parseRepeat()

      case "BREAK" =>
        parseBreak()

      case "GOTO" =>
        parseGoto()

      case "DOUBLECOLON" =>
        parseLabel()

      case _ => null
    }
  }

  def parseBreak(): BreakStat = {
    eat("BREAK")
    BreakStat()
  }

  def parseGoto(): GotoStat = {
    eat("GOTO")
    val name = eat("NAME").value
    GotoStat(name)
  }

  def parseLabel(): LabelStat = {
    eat("DOUBLECOLON")
    val name = eat("NAME").value
    eat("DOUBLECOLON")
    LabelStat(name)
  }

  // parses: varlist '=' explist | functioncall

  /**
   * This handles the ambiguity between assignment and function call
   * Both start witth a prefixep. The solution is to parse that first, then
   * check what comes next. '=' ( assignment ) or ',' ( function call )
   *
   */
  def parseAssignOrCall(): Stat = {
    // println("parseAssignOrCall")
    val first = parsePrefixexp()
    // println("first " + first)
    first match {
      case FunctionCallExp(fc) =>
        // check if more vars or assignment
        if (lookahead != null && lookahead.kind == "COMMA") {
          throw new Exception("cannot assign to function call")
        } else if (lookahead != null && lookahead.kind == "ASSIGN") {
          throw new Exception("cannot assign to function call")
        } else {
          FunctionCallStat(fc)
        }
      case VarExp(v) =>
        if (lookahead != null && lookahead.kind == "COMMA") {
          // varlist = explist
          val vars = ListBuffer[Var](v)
          while (lookahead != null && lookahead.kind == "COMMA") {
            eat("COMMA")
            val next = parsePrefixexp()
            next match {
              case VarExp(v2) => vars += v2
              case _ => throw new Exception("expected var in varlist")
            }
          }
          eat("ASSIGN")
          val exps = parseExpList()
          AssignStat(vars.toList, exps)
        } else if (lookahead != null && lookahead.kind == "ASSIGN") {
          // single var = explist
          eat("ASSIGN")
          val exps = parseExpList()
          AssignStat(List(v), exps)
        } else {
          throw new Exception("expected assignment or function call")
        }
      case _ =>
        throw new Exception("expected var or function call")
    }
  }

  /**
   * from LUA BNF:
   * prefixexp ::= var | functioncall | '(' exp ')'
   * var ::= Name | prefixexp '[' exp ']' | prefixexp '.' Name
   *
   * Parses the base (a Name or a parenthesized expression) and then continues parsing any suffixes
   */
  def parsePrefixexp(): Exp = {
    // println("prefix")
    val base = if (lookahead.kind == "LPAREN") {
      eat("LPAREN")
      val exp = parseExp()
      eat("RPAREN")
      ParenExp(exp)
    } else {
      val name = eat("NAME").value
      VarExp(NameVar(name))
    }
    parsePrefixexpSuffixes(base)
  }

  /**
   * After we get the base, we check if there are more things after it like dots or brackets.
   * We keep going recursively until nothing more follows.
   *
   * Handles suffix chains for prefixexp:
   * - '.' Name
   * - '[' exp ']'
   * - args
   * - ':' Name args
   *
   * from LUA BNF
   * args ::= '(' [explist] ')' | tableconstructor | LiteralString
   */
  def parsePrefixexpSuffixes(prefix: Exp): Exp = {
    if (lookahead == null) return prefix

    lookahead.kind match {
      case "DOT" =>
        eat("DOT")
        val name = eat("NAME").value
        parsePrefixexpSuffixes(VarExp(DotVar(prefix, name)))

      case "LBRACKET" =>
        eat("LBRACKET")
        val index = parseExp()
        eat("RBRACKET")
        parsePrefixexpSuffixes(VarExp(IndexVar(prefix, index)))

      case "LPAREN" =>
        eat("LPAREN")
        val args = if (lookahead.kind != "RPAREN") parseExpList() else List()
        eat("RPAREN")
        parsePrefixexpSuffixes(FunctionCallExp(FunctionCall(prefix, None, args)))

      case "COLON" =>
        eat("COLON")
        val method = eat("NAME").value
        eat("LPAREN")
        val args = if (lookahead.kind != "RPAREN") parseExpList() else List()
        eat("RPAREN")
        parsePrefixexpSuffixes(FunctionCallExp(FunctionCall(prefix, Some(method), args)))

      case "LBRACE" =>
        val table = parseTableConstructor()
        parsePrefixexpSuffixes(FunctionCallExp(FunctionCall(prefix, None, List(table))))

      case "STRING" =>
        val str = eat("STRING").value
        parsePrefixexpSuffixes(FunctionCallExp(FunctionCall(prefix, None, List(LiteralString(str)))))

      case _ =>
        prefix
    }
  }

  def parseNameList(): List[String] = {
    // println("namelist")
    val names = ListBuffer(eat("NAME").value)
    while (lookahead != null && lookahead.kind == "COMMA") {
      eat("COMMA")
      names += eat("NAME").value
    }
    names.toList
  }

  /**
   * A list of expressions separated by commas.
   *
   * from LUA BNF
   * explist ::= exp {',' exp}
   */
  def parseExpList(): List[Exp] = {
    // println("explist")
    val exps = ListBuffer[Exp](parseExp())
    while (lookahead != null && lookahead.kind == "COMMA") {
      eat("COMMA")
      exps += parseExp()
    }
    exps.toList
  }

  /**
   * Entry point for parsing expressions. We start with the lowest precedence and go up.
   *
   * from LUA BNF:
   * exp ::= nil | false | true | Numeral | LiteralString | '...' |
   *         functiondef | prefixexp | tableconstructor | exp binop exp | unop exp
   *
   * Precedence (low to high): or, and, comparison, concat, add, mul, unary, power
   */
  def parseExp(): Exp = {
    parseOr()
  }

  /**
   * Or has the lowest priority so we parse it first in the chain.
   *
   * Lowest precedence: 'or' is left associative
   */
  def parseOr(): Exp = {
    var left = parseAnd()
    while (lookahead != null && lookahead.kind == "OR") {
      eat("OR")
      left = BinopExp(left, "or", parseAnd())
    }
    left
  }

  def parseAnd(): Exp = {
    var left = parseCompareExp()
    while (lookahead != null && lookahead.kind == "AND") {
      eat("AND")
      left = BinopExp(left, "and", parseCompareExp())
    }
    left
  }

  def parseCompareExp(): Exp = {
    var left = parseConcatExp()
    while (lookahead != null && isCompareOp()) {
      val op = lookahead.value
      eat(lookahead.kind)
      left = BinopExp(left, op, parseConcatExp())
    }
    left
  }

  def isCompareOp(): Boolean = {
    lookahead.kind match {
      case "LT" | "GT" | "LE" | "GE" | "EQ" | "NE" => true
      case _ => false
    }
  }

  /**
   * Concatenation is special because it groups to the right, so we use recursion instead of a loop.
   *
   * from LUA BNF:
   * concat ::= '..' is right associative
   * a..b..c parses as a..(b..c)
   */
  def parseConcatExp(): Exp = {
    var left = parseAddExp()
    if (lookahead != null && lookahead.kind == "DOTDOT") {
      eat("DOTDOT")
      BinopExp(left, "..", parseConcatExp())
    } else left
  }

  def parseAddExp(): Exp = {
    var left = parseMulExp()
    while (lookahead != null && (lookahead.kind == "PLUS" || lookahead.kind == "MINUS")) {
      val op = lookahead.value
      eat(lookahead.kind)
      left = BinopExp(left, op, parseMulExp())
    }
    left
  }

  def parseMulExp(): Exp = {
    var left = parseUnaryExp()
    while (lookahead != null && isMulOp()) {
      val op = lookahead.value
      eat(lookahead.kind)
      left = BinopExp(left, op, parseUnaryExp())
    }
    left
  }

  def isMulOp(): Boolean = {
    lookahead.kind match {
      case "STAR" | "SLASH" | "FLOORDIV" | "PERCENT" => true
      case _ => false
    }
  }

  def parseUnaryExp(): Exp = {
    if (lookahead != null && isUnaryOp()) {
      val op = lookahead.value
      eat(lookahead.kind)
      UnopExp(op, parseUnaryExp())
    } else {
      parsePowerExp()
    }
  }

  def isUnaryOp(): Boolean = {
    lookahead.kind match {
      case "MINUS" | "NOT" | "HASH" | "TILDE" => true
      case _ => false
    }
  }

  /**
   * from LUA BNF:
   * power ::= '^' is right associative
   * for exmaple -2^2 = -(2^2) = -4
   */
  def parsePowerExp(): Exp = {
    val left = parsePrimaryExp()
    if (lookahead != null && lookahead.kind == "CARET") {
      eat("CARET")
      BinopExp(left, "^", parseUnaryExp())
    } else left
  }

  def parsePrimaryExp(): Exp = {
    if (lookahead == null) return NilExp()
    // println(s"parsePrimaryExp: ${lookahead.kind} = ${lookahead.value}")

    lookahead.kind match {
      case "NIL" =>
        eat("NIL")
        NilExp()

      case "TRUE" =>
        eat("TRUE")
        TrueExp()

      case "FALSE" =>
        eat("FALSE")
        FalseExp()

      case "NUMBER" =>
        val token = eat("NUMBER")
        Numeral(token.value)

      case "STRING" =>
        val token = eat("STRING")
        LiteralString(token.value)

      case "DOTDOTDOT" =>
        eat("DOTDOTDOT")
        VarargExp()

      case "LBRACE" =>
        parseTableConstructor()

      case "FUNCTION" =>
        eat("FUNCTION")
        FunctionDefExp(parseFuncBody())

      case "NAME" =>
        val name = eat("NAME").value
        parseSuffixes(VarExp(NameVar(name)))

      case "LPAREN" =>
        eat("LPAREN")
        val exp = parseExp()
        eat("RPAREN")
        parseSuffixes(ParenExp(exp))

      case _ =>
        NilExp()
    }
  }

  // handle suffixes like dot access, indexing, calls, method calls
  def parseSuffixes(prefix: Exp): Exp = {
    if (lookahead == null) return prefix

    lookahead.kind match {
      case "DOT" =>
        eat("DOT")
        val name = eat("NAME").value
        parseSuffixes(VarExp(DotVar(prefix, name)))

      case "LBRACKET" =>
        eat("LBRACKET")
        val index = parseExp()
        eat("RBRACKET")
        parseSuffixes(VarExp(IndexVar(prefix, index)))

      case "LPAREN" =>
        eat("LPAREN")
        val args = if (lookahead.kind != "RPAREN") parseExpList() else List()
        eat("RPAREN")
        parseSuffixes(FunctionCallExp(FunctionCall(prefix, None, args)))

      case "COLON" =>
        eat("COLON")
        val method = eat("NAME").value
        eat("LPAREN")
        val args = if (lookahead.kind != "RPAREN") parseExpList() else List()
        eat("RPAREN")
        parseSuffixes(FunctionCallExp(FunctionCall(prefix, Some(method), args)))

      case "LBRACE" =>
        // table constructor as argument
        val table = parseTableConstructor()
        parseSuffixes(FunctionCallExp(FunctionCall(prefix, None, List(table))))

      case "STRING" =>
        // string literal as argument
        val str = eat("STRING").value
        parseSuffixes(FunctionCallExp(FunctionCall(prefix, None, List(LiteralString(str)))))

      case _ =>
        prefix
    }
  }

  /**
   * If then else parttern matching. There can be multiple elsif
   * in the middle.
   *
   * from LUA BNF:
   * ifstat ::= 'if' exp 'then' block {'elseif' exp 'then' block} ['else' block] 'end'
   */
  def parseIf(): IfStat = {
    // println("parseIf")
    eat("IF")
    val cond = parseExp()
    eat("THEN")
    val thenBlock = parseBlock()

    val elseifs = ListBuffer[ElseIf]()
    while (lookahead != null && lookahead.kind == "ELSEIF") {
      // println("elseif branch")
      eat("ELSEIF")
      val c = parseExp()
      eat("THEN")
      val b = parseBlock()
      elseifs += ElseIf(c, b)
    }

    val elseBlock = if (lookahead != null && lookahead.kind == "ELSE") {
      eat("ELSE")
      Some(parseBlock())
    } else None

    eat("END")
    IfStat(cond, thenBlock, elseifs.toList, elseBlock)
  }

  def parseWhile(): WhileStat = {
    // println("while")
    eat("WHILE")
    val cond = parseExp()
    eat("DO")
    val block = parseBlock()
    eat("END")
    WhileStat(cond, block)
  }

  /**
   * Lua has two kinds of for loops. We look at what comes after the first name to know which one
   * we are dealing with.
   *
   * from LUA BNF:
   * forstat ::= 'for' Name '=' exp ',' exp [',' exp] 'do' block 'end'   (numeric)
   *           | 'for' namelist 'in' explist 'do' block 'end'            (generic)
   *
   * Distinguishes by checking for '=' after first Name
   */
  def parseFor(): Stat = {
    // println("parseFor")
    eat("FOR")
    val name = eat("NAME").value

    if (lookahead.kind == "ASSIGN") {
      // numeric for: for i = 1, 10 do
      // println("numeric for")
      eat("ASSIGN")
      val start = parseExp()
      eat("COMMA")
      val end = parseExp()
      val step = if (lookahead != null && lookahead.kind == "COMMA") {
        eat("COMMA")
        Some(parseExp())
      } else None
      eat("DO")
      val block = parseBlock()
      eat("END")
      ForNumStat(name, start, end, step, block)
    } else {
      // generic for loop
      // println("generic for")
      val names = ListBuffer(name)
      while (lookahead != null && lookahead.kind == "COMMA") {
        eat("COMMA")
        names += eat("NAME").value
      }
      eat("IN")
      val exps = parseExpList()
      eat("DO")
      val block = parseBlock()
      eat("END")
      ForInStat(names.toList, exps, block)
    }
  }

  def parseFunction(): FunctionStat = {
    // println("func")
    eat("FUNCTION")
    val funcName = parseFuncName()
    val body = parseFuncBody()
    FunctionStat(funcName, body)
  }

  def parseFuncName(): FuncName = {
    // println("funcname")
    val names = ListBuffer(eat("NAME").value)
    while (lookahead != null && lookahead.kind == "DOT") {
      eat("DOT")
      names += eat("NAME").value
    }
    val method = if (lookahead != null && lookahead.kind == "COLON") {
      eat("COLON")
      Some(eat("NAME").value)
    } else None
    FuncName(names.toList, method)
  }

  /**
   * Parses the parameters and the body of a function. Can have vararg at the end.
   *
   * from LUA BNF:
   * funcbody ::= '(' [parlist] ')' block 'end'
   * parlist ::= namelist [',' '...'] | '...'
   */
  def parseFuncBody(): FuncBody = {
    // println("parseFuncBody")
    eat("LPAREN")
    val params = ListBuffer[String]()
    var vararg = false
    if (lookahead != null && lookahead.kind != "RPAREN") {
      if (lookahead.kind == "DOTDOTDOT") {
        eat("DOTDOTDOT")
        vararg = true
      } else {
        params += eat("NAME").value
        while (lookahead != null && lookahead.kind == "COMMA") {
          eat("COMMA")
          if (lookahead.kind == "DOTDOTDOT") {
            eat("DOTDOTDOT")
            vararg = true
          } else {
            params += eat("NAME").value
          }
        }
      }
    }
    eat("RPAREN")
    // println("params " + params)
    val block = parseBlock()
    eat("END")
    FuncBody(params.toList, vararg, block)
  }

  def parseLocal(): Stat = {
    // println("local")
    eat("LOCAL")
    if (lookahead != null && lookahead.kind == "FUNCTION") {
      eat("FUNCTION")
      val name = eat("NAME").value
      val body = parseFuncBody()
      LocalFunctionStat(name, body)
    } else {
      val names = ListBuffer(eat("NAME").value)
      while (lookahead != null && lookahead.kind == "COMMA") {
        eat("COMMA")
        names += eat("NAME").value
      }
      val exps = if (lookahead != null && lookahead.kind == "ASSIGN") {
        eat("ASSIGN")
        parseExpList()
      } else List()
      LocalStat(names.toList, exps)
    }
  }

  def parseDo(): DoStat = {
    // println("do block")
    eat("DO")
    val block = parseBlock()
    eat("END")
    DoStat(block)
  }

  def parseRepeat(): RepeatStat = {
    // println("repeat")
    eat("REPEAT")
    val block = parseBlock()
    eat("UNTIL")
    val cond = parseExp()
    RepeatStat(block, cond)
  }

  /**
   * Tables are like arrays and dictionaries mixed together. Fields can have different formats.
   *
   * from LUA BNF:
   * tableconstructor ::= '{' [fieldlist] '}'
   * fieldlist ::= field {fieldsep field} [fieldsep]
   * fieldsep ::= ',' | ';'
   */
  def parseTableConstructor(): TableConstructor = {
    // println("table")
    eat("LBRACE")
    val fields = ListBuffer[Field]()
    while (lookahead != null && lookahead.kind != "RBRACE") {
      fields += parseField()
      // seperator is optional
      if (lookahead != null && (lookahead.kind == "COMMA" || lookahead.kind == "SEMICOLON")) {
        eat(lookahead.kind)
      }
    }
    eat("RBRACE")
    // println("table fields " + fields.size)
    TableConstructor(fields.toList)
  }

  /**
   * A field can be [exp]=exp, name=exp, or just an expression by itself.
   *
   * from LUA BNF:
   * field ::= '[' exp ']' '=' exp | Name '=' exp | exp
   */
  def parseField(): Field = {
    // println("field")
    if (lookahead.kind == "LBRACKET") {
      // bracket key field
      eat("LBRACKET")
      val key = parseExp()
      eat("RBRACKET")
      eat("ASSIGN")
      val value = parseExp()
      ExpKeyField(key, value)
    } else if (lookahead.kind == "NAME") {
      val name = eat("NAME").value
      if (lookahead != null && lookahead.kind == "ASSIGN") {
        // name = exp
        eat("ASSIGN")
        val value = parseExp()
        NameKeyField(name, value)
      } else {
        // just exp, the name we ate is actually a variable reference
        ValueField(VarExp(NameVar(name)))
      }
    } else {
      ValueField(parseExp())
    }
  }

  /**
   * Helper function to consume tokens. If the token is not what we expect, we throw an error.
   *
   * Consumes token if it matches expected type, advances lookahead
   * Throws exception on mismatch (syntax error)
   */
  def eat(tokenType: String): Token = {
    val token = lookahead
    if (token == null) {
      throw new Exception("unexpected end of input")
    }
    if (token.kind != tokenType) {
      throw new Exception("expected " + tokenType + " but got " + token.kind)
    }
    lookahead = tokenizer.getNextToken()
    token
  }
}