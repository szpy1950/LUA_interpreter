trait ASTNode {
  def toJson(indent: Int = 0): String
}

case class Chunk(block: Block) extends ASTNode {
  def toJson(indent: Int = 0): String = {
    val spaces = "  " * indent
    spaces + "{\n" +
      spaces + "  \"type\": \"chunk\",\n" +
      spaces + "  \"block\": " + block.toJson(indent + 1).trim + "\n" +
      spaces + "}"
  }
}

case class Block(stat: List[ASTNode]) extends ASTNode {
  def toJson(indent: Int = 0): String = {
    val spaces = "  " * indent
    val statJsons = stat.map(s => s.toJson(indent + 1).trim).mkString(",\n" + spaces + "    ")
    spaces + "{\n" +
      spaces + "  \"type\": \"block\",\n" +
      spaces + "  \"stat\": [\n" +
      spaces + "    " + statJsons + "\n" +
      spaces + "  ]\n" +
      spaces + "}"
  }
}

case class Stat(varlist: VarList, explist: ExpList) extends ASTNode {
  def toJson(indent: Int = 0): String = {
    val spaces = "  " * indent
    spaces + "{\n" +
      spaces + "  \"type\": \"stat\",\n" +
      spaces + "  \"varlist\": " + varlist.toJson(indent + 1).trim + ",\n" +
      spaces + "  \"explist\": " + explist.toJson(indent + 1).trim + "\n" +
      spaces + "}"
  }
}

case class VarList(vars: List[Var]) extends ASTNode {
  def toJson(indent: Int = 0): String = {
    val spaces = "  " * indent
    val varJsons = vars.map(v => v.toJson(indent + 1).trim).mkString(",\n" + spaces + "    ")
    spaces + "{\n" +
      spaces + "  \"type\": \"varlist\",\n" +
      spaces + "  \"var\": [\n" +
      spaces + "    " + varJsons + "\n" +
      spaces + "  ]\n" +
      spaces + "}"
  }
}

case class Var(name: String) extends ASTNode {
  def toJson(indent: Int = 0): String = {
    val spaces = "  " * indent
    spaces + "{\n" +
      spaces + "  \"type\": \"var\",\n" +
      spaces + "  \"name\": \"" + name + "\"\n" +
      spaces + "}"
  }
}

case class ExpList(expressions: List[ASTNode]) extends ASTNode {
  def toJson(indent: Int = 0): String = {
    val spaces = "  " * indent
    val expJsons = expressions.map(e => e.toJson(indent + 1).trim).mkString(",\n" + spaces + "    ")
    spaces + "{\n" +
      spaces + "  \"type\": \"explist\",\n" +
      spaces + "  \"exp\": [\n" +
      spaces + "    " + expJsons + "\n" +
      spaces + "  ]\n" +
      spaces + "}"
  }
}


case class LiteralString(value: String) extends ASTNode {
  def toJson(indent: Int = 0): String = {
    val spaces = "  " * indent
    spaces + "{\n" +
      spaces + "  \"type\": \"LiteralString\",\n" +
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