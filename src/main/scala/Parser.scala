// parts created and verified with the assistance from Claude AI

import scala.collection.mutable.ListBuffer

class Parser() {
  var tokenizer: Tokenizer = null
  var lookahead: Token = null

  def parse(code: String): Chunk = {
    tokenizer = Tokenizer(code)
    lookahead = tokenizer.getNextToken()
    parseChunk()
  }

  def parseChunk(): Chunk = {
    Chunk(parseBlock())
  }

  def parseBlock(): Block = {
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

  def parseStat(): Stat = {
    if (lookahead == null) return null

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
  def parseAssignOrCall(): Stat = {
    val first = parsePrefixexp()
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

  // prefixexp ::= var | functioncall | '(' exp ')'
  // var ::= Name | prefixexp '[' exp ']' | prefixexp '.' Name
  // functioncall ::= prefixexp args | prefixexp ':' Name args
  def parsePrefixexp(): Exp = {
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
        val table = parseTable()
        parsePrefixexpSuffixes(FunctionCallExp(FunctionCall(prefix, None, List(table))))

      case "STRING" =>
        val str = eat("STRING").value
        parsePrefixexpSuffixes(FunctionCallExp(FunctionCall(prefix, None, List(LiteralString(str)))))

      case _ =>
        prefix
    }
  }

  def parseNameList(): List[String] = {
    val names = ListBuffer(eat("NAME").value)
    while (lookahead != null && lookahead.kind == "COMMA") {
      eat("COMMA")
      names += eat("NAME").value
    }
    names.toList
  }

  def parseExpList(): List[Exp] = {
    val exps = ListBuffer[Exp](parseExp())
    while (lookahead != null && lookahead.kind == "COMMA") {
      eat("COMMA")
      exps += parseExp()
    }
    exps.toList
  }

  def parseExp(): Exp = {
    parseOrExp()
  }

  // or has lowest precedence
  def parseOrExp(): Exp = {
    var left = parseAndExp()
    while (lookahead != null && lookahead.kind == "OR") {
      eat("OR")
      left = BinopExp(left, "or", parseAndExp())
    }
    left
  }

  def parseAndExp(): Exp = {
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

  def parsePowerExp(): Exp = {
    val left = parsePrimaryExp()
    if (lookahead != null && lookahead.kind == "CARET") {
      eat("CARET")
      BinopExp(left, "^", parseUnaryExp())
    } else left
  }

  def parsePrimaryExp(): Exp = {
    if (lookahead == null) return NilExp()

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
        parseTable()

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

  // handle .name, [exp], (args), :method(args) suffixes
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
        val table = parseTable()
        parseSuffixes(FunctionCallExp(FunctionCall(prefix, None, List(table))))

      case "STRING" =>
        // string literal as argument
        val str = eat("STRING").value
        parseSuffixes(FunctionCallExp(FunctionCall(prefix, None, List(LiteralString(str)))))

      case _ =>
        prefix
    }
  }

  def parseIf(): IfStat = {
    eat("IF")
    val cond = parseExp()
    eat("THEN")
    val thenBlock = parseBlock()

    val elseifs = ListBuffer[ElseIf]()
    while (lookahead != null && lookahead.kind == "ELSEIF") {
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
    eat("WHILE")
    val cond = parseExp()
    eat("DO")
    val block = parseBlock()
    eat("END")
    WhileStat(cond, block)
  }

  def parseFor(): Stat = {
    eat("FOR")
    val name = eat("NAME").value

    if (lookahead.kind == "ASSIGN") {
      // for i = 1, 10 do
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
      // for k, v in pairs(t) do
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
    eat("FUNCTION")
    val funcName = parseFuncName()
    val body = parseFuncBody()
    FunctionStat(funcName, body)
  }

  def parseFuncName(): FuncName = {
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

  def parseFuncBody(): FuncBody = {
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
    val block = parseBlock()
    eat("END")
    FuncBody(params.toList, vararg, block)
  }

  def parseLocal(): Stat = {
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
    eat("DO")
    val block = parseBlock()
    eat("END")
    DoStat(block)
  }

  def parseRepeat(): RepeatStat = {
    eat("REPEAT")
    val block = parseBlock()
    eat("UNTIL")
    val cond = parseExp()
    RepeatStat(block, cond)
  }

  def parseTable(): TableConstructor = {
    eat("LBRACE")
    val fields = ListBuffer[Field]()
    while (lookahead != null && lookahead.kind != "RBRACE") {
      fields += parseField()
      if (lookahead != null && (lookahead.kind == "COMMA" || lookahead.kind == "SEMICOLON")) {
        eat(lookahead.kind)
      }
    }
    eat("RBRACE")
    TableConstructor(fields.toList)
  }

  def parseField(): Field = {
    if (lookahead.kind == "LBRACKET") {
      // [exp] = exp
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
        // just exp
        ValueField(VarExp(NameVar(name)))
      }
    } else {
      ValueField(parseExp())
    }
  }

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