class Parser() {
  var tokenizer: Tokenizer = null
  var lookahead: Token = null

  def parse(code: String): ASTNode = {
    tokenizer = Tokenizer(code)
    lookahead = tokenizer.getNextToken()
    Chunk()
  }

  def Chunk(): Chunk = {
    return new Chunk(Exp())
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