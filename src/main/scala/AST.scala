trait ASTNode {
  def toJson(indent: Int = 0): String
}

case class Chunk(body: ASTNode) extends ASTNode {
  def toJson(indent: Int = 0): String = {
    val spaces = "  " * indent
    spaces + "{\n" +
      spaces + "  \"type\": \"Chunk\",\n" +
      spaces + "  \"body\": " + body.toJson(indent + 1).trim + "\n" +
      spaces + "}"
  }
}


case class LiteralString(value: String) extends ASTNode {
  def toJson(indent: Int = 0): String = {
    val spaces = "  " * indent
    spaces + "{\n" +
      spaces + "  \"type\": \"String\",\n" +
      spaces + "  \"value\": \"" + value + "\"\n" +
      spaces + "}"
  }
}

case class Numeral(value: Int) extends ASTNode {
  def toJson(indent: Int = 0): String = {
    val spaces = "  " * indent
    spaces + "{\n" +
      spaces + "  \"type\": \"Numeral\",\n" +
      spaces + "  \"value\": " + value + "\n" +
      spaces + "}"
  }
}