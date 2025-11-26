class Parser() {
  var tokenizer: Tokenizer = null
  var lookahead: Token = null

  def parse(code: String): ASTNode = {
    tokenizer = Tokenizer(code)
    lookahead = tokenizer.getNextToken()
    Chunk()
  }

  def Chunk(): Chunk = {
    return new Chunk(Block())
  }

  def Block(): Block = {
    var statements = List[ASTNode]()
    while (lookahead != null) {
      statements = statements :+ Stat()
    }
    new Block(statements)
  }

  def Stat(): ASTNode = {
    lookahead.kind match {
      case "SEMICOLON" =>
        eat("SEMICOLON")
        Stat()
      case "NAME" =>
        val vars = VarList()
        eat("ASSIGN")
        val exps = ExpList()
        new Stat(vars, exps)
    }
  }

  def VarList(): VarList = {
    var vars = List(Var())
    while (lookahead != null && lookahead.kind == "COMMA") {
      eat("COMMA")
      vars = vars :+ Var()
    }
    new VarList(vars)
  }

  def Var(): Var = {
    val token = eat("NAME")
    new Var(token.value)
  }

  def ExpList(): ExpList = {
    var expressions = List(Exp())
    while (lookahead != null && lookahead.kind == "COMMA") {
      eat("COMMA")
      expressions = expressions :+ Exp()
    }
    new ExpList(expressions)
  }

  def Exp(): ASTNode = {
    lookahead.kind match {
      case "NUMBER" => Numeral()
      case "STRING" => LiteralString()
    }
  }

  def Numeral(): Numeral = {
    val token = eat("NUMBER")
    new Numeral(token.value.toInt)
  }

  def LiteralString(): LiteralString = {
    val token = eat("STRING")
    new LiteralString(token.value)
  }

  def eat(tokenType: String): Token = {
    val token = lookahead

    if (token == null) {
      throw new Exception("Parsing error: wrong end of input")
    }

    if (token.kind != tokenType) {
      throw new Exception("Parsing error: wrong token type: " + token.kind)
    }

    lookahead = tokenizer.getNextToken()

    token
  }
}