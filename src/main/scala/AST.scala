// created and verified with the assistance from Claude AI

import upickle.default.*

// root of the AST
case class Chunk(block: Block) derives ReadWriter

// block contains statements and optional return
case class Block(stats: List[Stat], retstat: Option[RetStat] = None) derives ReadWriter

// return statement
case class RetStat(explist: List[Exp]) derives ReadWriter

// all statement types
sealed trait Stat derives ReadWriter

case class AssignStat(varlist: List[Var], explist: List[Exp]) extends Stat
case class FunctionCallStat(call: FunctionCall) extends Stat
case class LabelStat(name: String) extends Stat
case class BreakStat() extends Stat
case class GotoStat(name: String) extends Stat
case class DoStat(block: Block) extends Stat
case class WhileStat(condition: Exp, block: Block) extends Stat
case class RepeatStat(block: Block, condition: Exp) extends Stat
case class IfStat(condition: Exp, thenBlock: Block, elseifs: List[ElseIf], elseBlock: Option[Block]) extends Stat
case class ForNumStat(name: String, start: Exp, end: Exp, step: Option[Exp], block: Block) extends Stat
case class ForInStat(names: List[String], explist: List[Exp], block: Block) extends Stat
case class FunctionStat(name: FuncName, body: FuncBody) extends Stat
case class LocalFunctionStat(name: String, body: FuncBody) extends Stat
case class LocalStat(names: List[String], explist: List[Exp]) extends Stat

// helper for if elseif chains
case class ElseIf(condition: Exp, block: Block) derives ReadWriter

// function name like foo.bar:baz
case class FuncName(names: List[String], method: Option[String]) derives ReadWriter

// function body - params can be empty, vararg is for ... arguements
case class FuncBody(params: List[String], vararg: Boolean, block: Block) derives ReadWriter

// function call - method is for : syntax like obj:method()
case class FunctionCall(prefix: Exp, method: Option[String], args: List[Exp]) derives ReadWriter


// all variable types
sealed trait Var derives ReadWriter

case class NameVar(name: String) extends Var
case class IndexVar(prefix: Exp, index: Exp) extends Var  // tbl[key]
case class DotVar(prefix: Exp, name: String) extends Var  // tbl.key

// all expression types
sealed trait Exp derives ReadWriter

case class NilExp() extends Exp
case class FalseExp() extends Exp
case class TrueExp() extends Exp
case class Numeral(value: String) extends Exp  // kept as string to preserve format
case class LiteralString(value: String) extends Exp
case class VarargExp() extends Exp
case class FunctionDefExp(body: FuncBody) extends Exp
case class VarExp(v: Var) extends Exp
case class FunctionCallExp(call: FunctionCall) extends Exp
case class ParenExp(exp: Exp) extends Exp
case class TableConstructor(fields: List[Field]) extends Exp
case class BinopExp(left: Exp, op: String, right: Exp) extends Exp
case class UnopExp(op: String, exp: Exp) extends Exp

// table field types
sealed trait Field derives ReadWriter

case class ExpKeyField(key: Exp, value: Exp) extends Field     // [exp] = exp
case class NameKeyField(name: String, value: Exp) extends Field // name = exp
case class ValueField(value: Exp) extends Field                 // just exp