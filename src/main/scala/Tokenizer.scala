// parts created with the assistance from Claude AI

class Tokenizer(string: String) {
  var cursor: Int = 0

  // all the token patterns, order matters here
  val tokenPatterns: List[(String, String)] = List(
    // skip whitespace
    (null, "^\\s+"),

    // skip comments
    (null, "^--\\[\\[([\\s\\S]*?)\\]\\]"),
    (null, "^--[^\\n]*"),
    // TODO handle long strings

    // keywords, need to come before NAME or they get matched as names
    ("AND", "^and\\b"),
    ("BREAK", "^break\\b"),
    ("DO", "^do\\b"),
    ("ELSE", "^else\\b"),
    ("ELSEIF", "^elseif\\b"),
    ("END", "^end\\b"),
    ("FALSE", "^false\\b"),
    ("FOR", "^for\\b"),
    ("FUNCTION", "^function\\b"),
    ("GOTO", "^goto\\b"),
    ("IF", "^if\\b"),
    ("IN", "^in\\b"),
    ("LOCAL", "^local\\b"),
    ("NIL", "^nil\\b"),
    ("NOT", "^not\\b"),
    ("OR", "^or\\b"),
    ("REPEAT", "^repeat\\b"),
    ("RETURN", "^return\\b"),
    ("THEN", "^then\\b"),
    ("TRUE", "^true\\b"),
    ("UNTIL", "^until\\b"),
    ("WHILE", "^while\\b"),

    // numbers, decimal first so 3.14 doesnt match as 3 then .14
    ("NUMBER", "^\\d+\\.\\d+"),
    ("NUMBER", "^\\d+"),

    // strings
    ("STRING", "^\"[^\"]*\""),
    ("STRING", "^'[^']*'"),

    // multi char operators, these MUST come before single char ones
    ("DOUBLECOLON", "^::"),
    ("DOTDOTDOT", "^\\.\\.\\."),
    ("DOTDOT", "^\\.\\."),
    ("EQ", "^=="),
    ("NE", "^~="),
    ("LE", "^<="),
    ("GE", "^>="),
    ("SHL", "^<<"),
    ("SHR", "^>>"),
    ("FLOORDIV", "^//"),

    // single char operators
    ("PLUS", "^\\+"),
    ("MINUS", "^-"),
    ("STAR", "^\\*"),
    ("SLASH", "^/"),
    ("PERCENT", "^%"),
    ("CARET", "^\\^"),
    ("HASH", "^#"),
    ("AMPERSAND", "^&"),
    ("PIPE", "^\\|"),
    ("TILDE", "^~"),
    ("LT", "^<"),
    ("GT", "^>"),
    ("ASSIGN", "^="),

    // delimiters
    ("LPAREN", "^\\("),
    ("RPAREN", "^\\)"),
    ("LBRACKET", "^\\["),
    ("RBRACKET", "^\\]"),
    ("LBRACE", "^\\{"),
    ("RBRACE", "^\\}"),
    ("COMMA", "^,"),
    ("SEMICOLON", "^;"),
    ("COLON", "^:"),
    ("DOT", "^\\."),

    // names, must be last
    ("NAME", "^[a-zA-Z_][a-zA-Z0-9_]*")
  )

  def hasMoreTokens(): Boolean = {
    cursor < string.length
  }

  def getNextToken(): Token = {
    if (!hasMoreTokens()) {
      return null
    }

    val slice = string.substring(cursor)

    for ((kind, pattern) <- tokenPatterns) {
      val regex = pattern.r
      val matched = regex.findFirstMatchIn(slice)
      if (matched.isDefined) {
        val value = matched.get.matched
        cursor += value.length
        // println(s"matched: $kind -> $value")
        kind match {
          case null => return getNextToken()
          case "STRING" => return Token(kind, value.substring(1, value.length - 1))
          case _ => return Token(kind, value)
        }
      }
    }

    // no match found
    throw new Exception(s"Tokenizer error: unexpected character '${slice.head}' at position $cursor")
  }
}