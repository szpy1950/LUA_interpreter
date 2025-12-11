// parts created with the assistance from Claude AI

/**
 * Tokenizer breaks the source code (string) into tokens and
 * takes the raw string and returns tokens one by one when getNextToken() is called
 * The cursor keeps track of the current position in the string
 */
class Tokenizer(string: String) {
  var cursor: Int = 0

  /**
   * List of all token patterns, order matters because patterns are checked in order
   * and the first match is used.
   */
  val tokenPatterns: List[(String, String)] = List(
    /** null means that we are skipping the token */
    (null, "^\\s+"),

    /** supporting multiline and single line comments */
    (null, "^--\\[\\[([\\s\\S]*?)\\]\\]"),
    (null, "^--[^\\n]*"),

    /** all keywords must come before NAME ( last one )
     * ^ esnures that the match starts at the current position
     * \b ensures the match ends at a word boundary
     * \\ is used to escape the \ character in the string for \b
     * */
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

    /** numbers, decimal first so 3.14 doesnt match as 3 then .14 */
    ("NUMBER", "^\\d+\\.\\d+"),
    ("NUMBER", "^\\d+"),

    /** strings with double or single quotes */
    ("STRING", "^\"[^\"]*\""),
    ("STRING", "^'[^']*'"),

    /** multi char operators must come before single char, otherwise == matches as = = */
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

    /** single char operators */
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

    /** delimiters */
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

    /** names must be last, starts with letter or underscore */
    ("NAME", "^[a-zA-Z_][a-zA-Z0-9_]*")
  )

  def hasMoreTokens(): Boolean = {
    cursor < string.length
  }

  /**
   * The function scans the input string and retursn the next token, or null if we reached the end of input.
   * It goes trhough all token patterns in order and then uses the first one that matches.
   *
   * If the matched pattern has kind = null ( like a whitespace or comment ) , it skips it and continues
   *
   * There is a special handling for STRING type: removing the quotes around it before returning
   *
   * A slice is used to select a substring starting at current position
   *
   * Then it moves the current position in the input past the part that was just matched
   *
   * If no pattern matches the current position , throws an exception
   */
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
        kind match {
          case null => return getNextToken()
          case "STRING" => return Token(kind, value.substring(1, value.length - 1))
          case _ => return Token(kind, value)
        }
      }
    }

    /** When no match is found , throws an exception error*/
    throw new Exception(s"Tokenizer error: unexpected character '${slice.head}' at position $cursor")
  }
}