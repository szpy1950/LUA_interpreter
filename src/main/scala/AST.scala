// created and verified with the assistance from Claude AI

/**
 * This file defines the nodes of the Abstract Syntax Tree
 * The parser makes this tree from the source code and the evaluator uses it
 * "derives ReadWriter" is for upickle JSON so it can be saved or exported the AST
 */

import upickle.default.*

/**
 * Root of the AST. Every Lua program is a chunk.
 * chunk ::= block
 */
case class Chunk(block: Block) derives ReadWriter

/**
 * A block is a sequence of statements, can end with a return
 * block ::= {stat} [retstat]
 */
case class Block(stats: List[Stat], retstat: Option[RetStat] = None) derives ReadWriter
case class RetStat(explist: List[Exp]) derives ReadWriter

/**
 * All the different statements in Lua
 * stat ::=  ‘;’ |
 *           varlist ‘=’ explist |
 *           functioncall |
 *           label |
 *           break |
 *           goto Name |
 *           do block end |
 *           while exp do block end |
 *           repeat block until exp |
 *           if exp then block {elseif exp then block} [else block] end |
 *           for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end |
 *           for namelist in explist do block end |
 *           function funcname funcbody |
 *           local function Name funcbody |
 *           local attnamelist [‘=’ explist]
 */
sealed trait Stat derives ReadWriter

// varlist ‘=’ explist
case class AssignStat(varlist: List[Var], explist: List[Exp]) extends Stat

// functioncall
case class FunctionCallStat(call: FunctionCall) extends Stat

// label ::= ‘::’ Name ‘::’
case class LabelStat(name: String) extends Stat

// break
case class BreakStat() extends Stat

// goto Name
case class GotoStat(name: String) extends Stat

// do block end
case class DoStat(block: Block) extends Stat

// while exp do block end
case class WhileStat(condition: Exp, block: Block) extends Stat

// repeat block until exp
case class RepeatStat(block: Block, condition: Exp) extends Stat

// if exp then block {elseif exp then block} [else block] end
case class IfStat(condition: Exp, thenBlock: Block, elseifs: List[ElseIf], elseBlock: Option[Block]) extends Stat

// for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end
case class ForNumStat(name: String, start: Exp, end: Exp, step: Option[Exp], block: Block) extends Stat

// for namelist in explist do block end
case class ForInStat(names: List[String], explist: List[Exp], block: Block) extends Stat

// function funcname funcbody
case class FunctionStat(name: FuncName, body: FuncBody) extends Stat

// local function Name funcbody
case class LocalFunctionStat(name: String, body: FuncBody) extends Stat

// local attnamelist [‘=’ explist]
case class LocalStat(names: List[String], explist: List[Exp]) extends Stat

// helper for if elseif chains
case class ElseIf(condition: Exp, block: Block) derives ReadWriter

// funcname ::= Name {‘.’ Name} [‘:’ Name]
case class FuncName(names: List[String], method: Option[String]) derives ReadWriter

// funcbody ::= ‘(’ [parlist] ‘)’ block end
case class FuncBody(params: List[String], vararg: Boolean, block: Block) derives ReadWriter

// functioncall ::= prefixexp args | prefixexp ‘:’ Name args
case class FunctionCall(prefix: Exp, method: Option[String], args: List[Exp]) derives ReadWriter

/**
 * Variables in Lua can be a simple name or table[key] access or table.name access
 * var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name
 */
sealed trait Var derives ReadWriter

case class NameVar(name: String) extends Var  // simple variable like x
case class IndexVar(prefix: Exp, index: Exp) extends Var  // tbl[key]
case class DotVar(prefix: Exp, name: String) extends Var  // tbl.key (sugar for tbl["key"])

/**
 * Expressions are things that produce a value when evaluated
 * exp ::= nil | false | true | Numeral | LiteralString | ‘...’ | functiondef |
 *        prefixexp | tableconstructor | exp binop exp | unop exp
 */
sealed trait Exp derives ReadWriter
case class NilExp() extends Exp
case class FalseExp() extends Exp
case class TrueExp() extends Exp
case class Numeral(value: String) extends Exp
case class LiteralString(value: String) extends Exp
case class VarargExp() extends Exp  // ... for variable arguments
case class FunctionDefExp(body: FuncBody) extends Exp
case class VarExp(v: Var) extends Exp
case class FunctionCallExp(call: FunctionCall) extends Exp
case class ParenExp(exp: Exp) extends Exp
case class TableConstructor(fields: List[Field]) extends Exp
case class BinopExp(left: Exp, op: String, right: Exp) extends Exp
case class UnopExp(op: String, exp: Exp) extends Exp

/**
 * Table constructor fields
 * field ::=  [ exp ] = exp | Name = exp | exp
 */
sealed trait Field derives ReadWriter

case class ExpKeyField(key: Exp, value: Exp) extends Field
case class NameKeyField(name: String, value: Exp) extends Field
case class ValueField(value: Exp) extends Field
