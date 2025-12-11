// Parts created and verified with the assistance from Claude AI

/**
 * Evaluator for the Lua interpreter
 * takes the AST from the parser and executes it
 * handles expressions, statements, function calls and variable scoping
 */

import scala.collection.mutable

/**
 * All runtime values in Lua
 * nil, boolean, number, string, table and function
 * builtin represents a native function provided byt the runtime:
 * - print
 * - type
 * - tonumber
 * - tostring
 * - error
 */
sealed trait LuaVal {
  def show: String = this match {
    case LuaNil => "nil"
    case LuaBool(v) => v.toString
    case LuaNum(v) => if (v == v.toLong) v.toLong.toString else v.toString
    case LuaStr(v) => v
    case LuaTable(_) => "table"
    case LuaFunc(_, _, _, _) => "function"
    case LuaBuiltin(_, _) => "function"
  }
}

case object LuaNil extends LuaVal
case class LuaBool(v: Boolean) extends LuaVal
case class LuaNum(v: Double) extends LuaVal
case class LuaStr(v: String) extends LuaVal
case class LuaTable(map: mutable.Map[LuaVal, LuaVal]) extends LuaVal
case class LuaFunc(params: List[String], vararg: Boolean, body: Block, closure: Env) extends LuaVal
case class LuaBuiltin(name: String, fn: List[LuaVal] => List[LuaVal]) extends LuaVal

class EvalError(msg: String) extends Exception(msg)

class ReturnException(val values: List[LuaVal]) extends Exception

/** Used to handle break statements, caught in loops */
class BreakException extends Exception

/**
 * Environment for variable scoping in Lua
 *
 * Each env has its own variables and a parent for outer scope
 * A closure is a funciton that remembers the environment where it was created.
 *
 * This allows the function to get access to variable from the scope even when called in a
 * different scope or even later in the program.
 */
class Env(var vars: mutable.Map[String, LuaVal], val parent: Option[Env]) {

  /** Retrieves a variable by name, goes to parent if not found locally */
  def get(name: String): LuaVal = {
    // println(s"get: $name")
    if (vars.contains(name)) vars(name)
    else parent match {
      case Some(p) => p.get(name)
      case None => LuaNil
    }
  }

  /**
   * Updates a variable if it exists in the scope, else calls define to create it
   * */
  def set(name: String, value: LuaVal): Unit = {
    if (vars.contains(name)) vars(name) = value
    else parent match {
      case Some(p) => p.set(name, value)
      case None => define(name, value)  // global by default
    }
  }

  /** Creates a new variable in the current scope */
  def define(name: String, value: LuaVal): Unit = {
    vars(name) = value
  }

  /** Creates a child environment for a new scope like function or block */
  def child(): Env = new Env(mutable.Map(), Some(this))
}

object Eval {

  /**
   * Recursively evaluates a LUA expression AST node and returns a luaVal
   *
   * turns expressions into values
   *
   * Uses as parameter the given environment to look up variables
   * */
  def eval(e: Exp, env: Env): LuaVal = e match {
    case NilExp() => LuaNil
    case TrueExp() => LuaBool(true)
    case FalseExp() => LuaBool(false)
    case Numeral(s) => LuaNum(s.toDouble)
    case LiteralString(s) => LuaStr(s)
    case ParenExp(inner) => eval(inner, env)
    case UnopExp(op, exp) => evalUnop(op, eval(exp, env))
    case BinopExp(l, op, r) => evalBinop(l, op, r, env)
    case VarExp(v) =>
      // println("varexp " + v)
      evalVar(v, env)
    case TableConstructor(fields) => evalTable(fields, env)
    case FunctionDefExp(body) => LuaFunc(body.params, body.vararg, body.block, env)
    case FunctionCallExp(call) =>
      val results = evalCall(call, env)
      // println("call result " + results)
      if (results.isEmpty) LuaNil else results.head
    case VarargExp() =>
      env.get("...") match {
        case LuaTable(m) =>
          val key = LuaNum(1)
          if (m.contains(key)) m(key) else LuaNil
        case _ => LuaNil
      }
  }

  /**
   * explist ::= exp {',' exp}
   *
   * Only the last expression can expand to multiple values
   * Example: local a, b, c = func() where func returns 3 values
   */
  def evalExpList(exps: List[Exp], env: Env): List[LuaVal] = {
    if (exps.isEmpty) return List()

    val init = exps.init.map(e => eval(e, env))
    val last = exps.last

    // last expression can return multiple values
    val lastVals = last match {
      case FunctionCallExp(call) => evalCall(call, env)
      case VarargExp() =>
        env.get("...") match {
          case LuaTable(m) =>
            var result = List[LuaVal]()
            var i = 1
            while (m.contains(LuaNum(i))) {
              result = result :+ m(LuaNum(i))
              i += 1
            }
            result
          case _ => List(LuaNil)
        }
      case _ => List(eval(last, env))
    }

    init ++ lastVals
  }

  /**
   * tableconstructor ::= '{' [fieldlist] '}'
   * field ::= '[' exp ']' '=' exp | Name '=' exp | exp
   *
   * Example: {1, 2, x = 3, [10] = "ten"}
   * Values without keys get auto incrementing integer index starting at 1
   */
  def evalTable(fields: List[Field], env: Env): LuaTable = {
    // println("in evaltable")
    val map = mutable.Map[LuaVal, LuaVal]()
    var idx = 1
    var fIdx = 0
    for (f <- fields) {
      val isLast = fIdx == fields.length - 1
      f match {
        case ExpKeyField(k, v) => map(eval(k, env)) = eval(v, env)
        case NameKeyField(n, v) => map(LuaStr(n)) = eval(v, env)
        case ValueField(v) =>
          if (isLast) {
            // last field can expand to multiple values
            val vals = v match {
              case FunctionCallExp(call) => evalCall(call, env)
              case VarargExp() =>
                env.get("...") match {
                  case LuaTable(m) =>
                    var result = List[LuaVal]()
                    var i = 1
                    while (m.contains(LuaNum(i))) {
                      result = result :+ m(LuaNum(i))
                      i += 1
                    }
                    result
                  case _ => List()
                }
              case _ => List(eval(v, env))
            }
            // println("vals " + vals)
            for (value <- vals) {
              map(LuaNum(idx)) = value
              idx += 1
            }
          } else {
            map(LuaNum(idx)) = eval(v, env)
            idx += 1
          }
      }
      fIdx += 1
    }
    LuaTable(map)
  }

  /**
   * var ::= Name | prefixexp '[' exp ']' | prefixexp '.' Name
   *
   * Example: x, t[1], t.name
   * Returns LuaNil if variable or key does not exist
   */
  def evalVar(v: Var, env: Env): LuaVal = v match {
    case NameVar(name) => env.get(name)
    case IndexVar(prefix, index) =>
      eval(prefix, env) match {
        case LuaTable(m) =>
          val key = eval(index, env)
          if (m.contains(key)) m(key) else LuaNil
        case _ => LuaNil
      }
    case DotVar(prefix, name) =>
      eval(prefix, env) match {
        case LuaTable(m) =>
          val key = LuaStr(name)
          if (m.contains(key)) m(key) else LuaNil
        case _ => LuaNil
      }
  }

  /**
   * var ::= Name | prefixexp '[' exp ']' | prefixexp '.' Name
   *
   * Example: x = 1, t[1] = 2, t.name = "foo"
   * Unlike evalVar this throws error if indexing a non table
   */
  def setVar(v: Var, value: LuaVal, env: Env): Unit = v match {
    case NameVar(name) => env.set(name, value)
    case IndexVar(prefix, index) =>
      eval(prefix, env) match {
        case LuaTable(m) => m(eval(index, env)) = value
        case _ => throw new EvalError("tried to index something that isnt a table")
      }
    case DotVar(prefix, name) =>
      eval(prefix, env) match {
        case LuaTable(m) => m(LuaStr(name)) = value
        case _ => throw new EvalError("tried to index something that isnt a table")
      }
  }

  /**
   * functioncall ::= prefixexp args | prefixexp ':' Name args
   *
   * Example: foo(x) or obj:method(x)
   * Method syntax obj:method(x) passes obj as first arg (self)
   * Function runs in child of its closure env not calling env
   */
  def evalCall(call: FunctionCall, env: Env): List[LuaVal] = {
    // println(s"evalCall: ${call}")
    val func = call.method match {
      case Some(method) =>
        eval(call.prefix, env) match {
          case LuaTable(m) =>
            val key = LuaStr(method)
            if (m.contains(key)) m(key) else LuaNil
          case _ => LuaNil
        }
      case None => eval(call.prefix, env)
    }

    // for method calls, add self as first argument
    val args = call.method match {
      case Some(_) => eval(call.prefix, env) :: evalExpList(call.args, env)
      case None => evalExpList(call.args, env)
    }

    // println("func is " + func)
    func match {
      case LuaFunc(params, vararg, body, closure) =>
        val funcEnv = closure.child()
        // println("params " + params + " args " + args)
        var i = 0
        for (p <- params) {
          if (i < args.length) funcEnv.define(p, args(i))
          else funcEnv.define(p, LuaNil)
          i += 1
        }
        if (vararg) {
          val extra = args.drop(params.length)
          val vt = mutable.Map[LuaVal, LuaVal]()
          var j = 0
          for (v <- extra) {
            vt(LuaNum(j + 1)) = v
            j += 1
          }
          funcEnv.define("...", LuaTable(vt))
        }
        try {
          execBlock(body, funcEnv)
          List()
        } catch {
          case r: ReturnException => r.values
        }
      case LuaBuiltin(_, fn) => fn(args)
      case _ => throw new EvalError("can't call this, not a function")
    }
  }

  /**
   * unop ::= '-' | not | '#' | '~'
   *
   * Length # counts sequential integer keys from 1 for tables
   */
  def evalUnop(op: String, v: LuaVal): LuaVal = op match {
    case "-" => LuaNum(-toNum(v))
    case "not" => LuaBool(!isTruthy(v))
    case "#" => v match {
      case LuaStr(s) => LuaNum(s.length)
      case LuaTable(m) =>
        // count sequential integer keys starting from 1
        var len = 0
        while (m.contains(LuaNum(len + 1))) len += 1
        LuaNum(len)
      case _ => throw new EvalError("can only get length of string or table")
    }
    case "~" => LuaNum(~toNum(v).toLong)
    case _ => throw new EvalError(s"unknown unary op: $op")
  }

  /**
   * binop ::= '+' | '-' | '*' | '/' | '//' | '^' | '%' |
   *           '&' | '~' | '|' | '>>' | '<<' | '..' |
   *           '<' | '<=' | '>' | '>=' | '==' | '~=' |
   *           and | or
   *
   * AND and OR return actual values: nil or 5 returns 5
   */
  def evalBinop(l: Exp, op: String, r: Exp, env: Env): LuaVal = op match {
    // short circuit evaluation for and/or
    case "and" => if (!isTruthy(eval(l, env))) eval(l, env) else eval(r, env)
    case "or" => if (isTruthy(eval(l, env))) eval(l, env) else eval(r, env)
    case _ =>
      val lv = eval(l, env)
      val rv = eval(r, env)
      op match {
        case "+" => LuaNum(toNum(lv) + toNum(rv))
        case "-" => LuaNum(toNum(lv) - toNum(rv))
        case "*" => LuaNum(toNum(lv) * toNum(rv))
        case "/" => LuaNum(toNum(lv) / toNum(rv))
        case "//" => LuaNum(Math.floor(toNum(lv) / toNum(rv)))
        // lua modulo is different from scala %
        // had to look this up online
        case "%" =>
          val a = toNum(lv)
          val b = toNum(rv)
          // first tried just a % b but that gave wrong results for negatives
          val r = a % b
          // this handles negative numbers correctly
          if ((r > 0 && b < 0) || (r < 0 && b > 0)) LuaNum(r + b)
          else LuaNum(r)
        case "^" => LuaNum(Math.pow(toNum(lv), toNum(rv)))
        case "<" => LuaBool(compare(lv, rv) < 0)
        case ">" => LuaBool(compare(lv, rv) > 0)
        case "<=" => LuaBool(compare(lv, rv) <= 0)
        case ">=" => LuaBool(compare(lv, rv) >= 0)
        case "==" => LuaBool(isEqual(lv, rv))
        case "~=" => LuaBool(!isEqual(lv, rv))
        case ".." => LuaStr(toStr(lv) + toStr(rv))
        case "&" => LuaNum(toNum(lv).toLong & toNum(rv).toLong)
        case "|" => LuaNum(toNum(lv).toLong | toNum(rv).toLong)
        case "~" => LuaNum(toNum(lv).toLong ^ toNum(rv).toLong)
        case "<<" => LuaNum(toNum(lv).toLong << toNum(rv).toInt)
        case ">>" => LuaNum(toNum(lv).toLong >> toNum(rv).toInt)
        case _ => throw new EvalError(s"unknown binary op: $op")
      }
  }

  /**
   * block ::= {stat} [retstat]
   *
   * Throws ReturnException on return, caught by evalCall
   */
  def execBlock(block: Block, env: Env): Unit = {
    for (s <- block.stats) exec(s, env)
    block.retstat.foreach { ret =>
      throw new ReturnException(evalExpList(ret.explist, env))
    }
  }

  /**
   * Runs one Lua statement.
   *
   * Turns statements into actions
   *
   * A statement can change the program in different ways:
   * - make new variables
   * - change existing variables
   * - call a function
   * - run control flow (if, while, for)
   * - open a new block with its own local variables
   *
   * This function checks what kind of statement it is
   * and then runs the matching code for that kind
   *
   * Scoping rule:
   * blocks, loops, and if elseif else parts run inside env.child(),
   * so they get their own local variables but can still read outer ones.
   */

  def exec(s: Stat, env: Env): Unit = s match {
    case LocalStat(names, exps) =>
      val vals = evalExpList(exps, env)
      var i = 0
      for (n <- names) {
        if (i < vals.length) env.define(n, vals(i))
        else env.define(n, LuaNil)
        i += 1
      }

    case AssignStat(vars, exps) =>
      val vals = evalExpList(exps, env)
      // println(s"assign: $vars = $vals")
      var i = 0
      for (v <- vars) {
        if (i < vals.length) setVar(v, vals(i), env)
        else setVar(v, LuaNil, env)
        i += 1
      }

    case FunctionCallStat(call) => evalCall(call, env)

    case IfStat(cond, thenBlock, elseifs, elseBlock) =>
      // println("if " + cond)
      if (isTruthy(eval(cond, env))) {
        execBlock(thenBlock, env.child())
      } else {
        var done = false
        for (elif <- elseifs if !done) {
          // println("elseif")
          if (isTruthy(eval(elif.condition, env))) {
            execBlock(elif.block, env.child())
            done = true
          }
        }
        if (!done) elseBlock.foreach(b => execBlock(b, env.child()))
      }

    case WhileStat(cond, block) =>
      try {
        while (isTruthy(eval(cond, env))) execBlock(block, env.child())
      } catch { case _: BreakException => }

    case RepeatStat(block, cond) =>
      try {
        var cont = true
        while (cont) {
          val loopEnv = env.child()
          execBlock(block, loopEnv)
          // condition is checked with block's scope so it can see local vars
          if (isTruthy(eval(cond, loopEnv))) cont = false
        }
      } catch { case _: BreakException => }

    case ForNumStat(name, start, end, step, block) =>
      // println("fornum " + name)
      val s = toNum(eval(start, env))
      val e = toNum(eval(end, env))
      val st = if (step.isDefined) toNum(eval(step.get, env)) else 1.0
      try {
        var i = s
        while ((st > 0 && i <= e) || (st < 0 && i >= e)) {
          val loopEnv = env.child()
          loopEnv.define(name, LuaNum(i))
          execBlock(block, loopEnv)
          i += st
        }
      } catch { case _: BreakException => }

    case ForInStat(names, explist, block) =>
      // generic for is complicated...
      // println("forin")
      val vals = explist.map(e => eval(e, env))
      val iter = if (vals.nonEmpty) vals.head else LuaNil
      val state = if (vals.length > 1) vals(1) else LuaNil
      var ctrl: LuaVal = if (vals.length > 2) vals(2) else LuaNil
      try {
        var cont = true
        while (cont) {
          val results = iter match {
            case LuaBuiltin(_, fn) => fn(List(state, ctrl))
            case LuaFunc(params, _, body, closure) =>
              val funcEnv = closure.child()
              if (params.nonEmpty) funcEnv.define(params.head, state)
              if (params.length > 1) funcEnv.define(params(1), ctrl)
              try { execBlock(body, funcEnv); List() }
              catch { case r: ReturnException => r.values }
            case _ => List()
          }
          // println("forin results " + results)
          if (results.isEmpty || results.head == LuaNil) cont = false
          else {
            ctrl = results.head
            val loopEnv = env.child()
            var i = 0
            for (n <- names) {
              if (i < results.length) loopEnv.define(n, results(i))
              else loopEnv.define(n, LuaNil)
              i += 1
            }
            execBlock(block, loopEnv)
          }
        }
      } catch { case _: BreakException => }

    case FunctionStat(funcName, body) =>
      val func = LuaFunc(body.params, body.vararg, body.block, env)
      if (funcName.names.length == 1 && funcName.method.isEmpty) {
        env.set(funcName.names.head, func)
      } else {
        // handle foo.bar.baz = function or foo:method = function
        var table = env.get(funcName.names.head)
        val rest = funcName.names.tail
        val toIterate = if (funcName.method.isDefined) rest else rest.dropRight(1)
        for (n <- toIterate) {
          table = table match {
            case LuaTable(m) =>
              val key = LuaStr(n)
              if (m.contains(key)) m(key) else LuaNil
            case _ => LuaNil
          }
        }
        val lastName = if (funcName.method.isDefined) funcName.method.get else funcName.names.last
        table match {
          case LuaTable(m) =>
            val actualFunc = if (funcName.method.isDefined) {
              // method syntax adds implicit self parameter
              LuaFunc("self" :: body.params, body.vararg, body.block, env)
            } else func
            m(LuaStr(lastName)) = actualFunc
          case _ =>
        }
      }

    case LocalFunctionStat(name, body) =>
      env.define(name, LuaFunc(body.params, body.vararg, body.block, env))

    case DoStat(block) => execBlock(block, env.child())
    case BreakStat() => throw new BreakException()
  }

  /** In Lua only nil and false are falsy, everything else is true */
  def isTruthy(v: LuaVal): Boolean = v match {
    case LuaNil => false
    case LuaBool(false) => false
    case _ => true
  }

  /** Converts a value to number, strings can be converted too */
  def toNum(v: LuaVal): Double = v match {
    case LuaNum(n) => n
    case LuaStr(s) =>
      val parsed = s.toDoubleOption
      if (parsed.isDefined) parsed.get
      else throw new EvalError(s"can't convert '$s' to number")
    case _ => throw new EvalError("expected a number")
  }

  def toStr(v: LuaVal): String = v match {
    case LuaStr(s) => s
    case LuaNum(n) => if (n == n.toLong) n.toLong.toString else n.toString
    case _ => throw new EvalError("expected string or number for concatenation")
  }

  def compare(a: LuaVal, b: LuaVal): Int = (a, b) match {
    case (LuaNum(x), LuaNum(y)) => x.compare(y)
    case (LuaStr(x), LuaStr(y)) => x.compare(y)
    case _ => throw new EvalError("can only compare numbers with numbers or strings with strings")
  }

  /**
   * Compares two Lua values for equality.
   *
   * - For simple types (nil, boolean, number, string), checks if the values are the same.
   * - For tables: two tables are equal only if they are the exact same object
   *   Lua does not compare the contents of tables by default
   * - All other combinations are considered unequal
   */
  def isEqual(a: LuaVal, b: LuaVal): Boolean = (a, b) match {
    case (LuaNil, LuaNil) => true
    case (LuaBool(x), LuaBool(y)) => x == y
    case (LuaNum(x), LuaNum(y)) => x == y
    case (LuaStr(x), LuaStr(y)) => x == y
    case (LuaTable(x), LuaTable(y)) => x eq y  // reference equality for tables
    case _ => false
  }

  /** Creates root env with no parent, this is where global variables and builtins live */
  def makeGlobalEnv(): Env = {
    val env = new Env(mutable.Map(), None)

    env.define("print", LuaBuiltin("print", args => {
      println(args.map(_.show).mkString("\t"))
      List()
    }))

    env.define("type", LuaBuiltin("type", args => {
      val arg = if (args.length > 0) args(0) else LuaNil
      val t = arg match {
        case LuaNil => "nil"
        case LuaBool(_) => "boolean"
        case LuaNum(_) => "number"
        case LuaStr(_) => "string"
        case LuaTable(_) => "table"
        case _ => "function"
      }
      List(LuaStr(t))
    }))

    env.define("tonumber", LuaBuiltin("tonumber", args => {
      if (args.length == 0) {
        List(LuaNil)
      } else {
        args(0) match {
          case LuaNum(n) => List(LuaNum(n))
          case LuaStr(s) =>
            val parsed = s.toDoubleOption
            if (parsed.isDefined) List(LuaNum(parsed.get)) else List(LuaNil)
          case _ => List(LuaNil)
        }
      }
    }))

    env.define("tostring", LuaBuiltin("tostring", args => {
      val arg = if (args.length > 0) args(0) else LuaNil
      List(LuaStr(arg.show))
    }))

    env.define("error", LuaBuiltin("error", args => {
      val msg = if (args.length > 0) args(0).show else "error"
      throw new EvalError(msg)
    }))

    env
  }
}