class Tokenizer(string: String) {
  var cursor: Int = 0

  val Spec: List[(String, String)] = List(
    (null, "^\\s+"),
    ("NUMBER", "^\\d+"),
    ("STRING", "^\"[^\"]*\""),
    ("COMMA", "^,"),
    ("SEMICOLON", "^;"),
    ("ASSIGN", "^="),
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

    for ((kind, pattern) <- Spec) {
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

    null
  }
}