class Tokenizer(string: String) {
  var cursor: Int = 0

  def hasMoreTokens(): Boolean = {
    cursor < string.length
  }

  def isEOF(): Boolean = {
    cursor >= string.length
  }

  def getNextToken(): Token = {
    if (!hasMoreTokens()) {
      return null
    }

    val slice = string.substring(cursor)

    // Numbers
    if (slice(0).isDigit) {
      var number = ""
      while (hasMoreTokens() && string(cursor).isDigit) {
        number = number + string(cursor)
        cursor = cursor + 1
      }
      return Token("NUMBER", number)
    }

    // Strings
    if (slice(0) == '"') {
      var s = ""
//      s = s + string(cursor)  // start "
      cursor = cursor + 1
      while (!isEOF() && string(cursor) != '"') {
        s = s + string(cursor)
        cursor = cursor + 1
      }
//      s = s + string(cursor)  // end "
      cursor = cursor + 1
      return Token("STRING", s)
    }

    null
  }
}