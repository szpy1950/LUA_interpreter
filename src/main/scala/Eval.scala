// Parts created and verified with the assistance from Claude AI

import scala.collection.mutable
import scala.util.Try

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
class BreakException extends Exception

// environment for variable scoping
class Env(var vars: mutable.Map[String, LuaVal], val parent: Option[Env]) {

  def get(name: String): LuaVal = {
    // println(s"get: $name")
    if (vars.contains(name)) vars(name)
    else parent match {
      case Some(p) => p.get(name)
      case None => LuaNil
    }
  }

  def set(name: String, value: LuaVal): Unit = {
    if (vars.contains(name)) vars(name) = value
    else parent match {
      case Some(p) => p.set(name, value)
      case None => vars(name) = value  // global by default
    }
  }

  def define(name: String, value: LuaVal): Unit = {
    vars(name) = value
  }

  def child(): Env = new Env(mutable.Map(), Some(this))
}

object Eval {

  // def isCallable(v: LuaVal): Boolean = v match {
  //   case LuaFunc(a, b, c, d) => true
  //   case LuaBuiltin(name, f) => true
  //   case x => false
  // }

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
        case LuaTable(m) => m.getOrElse(LuaNum(1), LuaNil)
        case _ => LuaNil
      }
  }

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

  def evalTable(fields: List[Field], env: Env): LuaTable = {
    // println("in evaltable")
    val map = mutable.Map[LuaVal, LuaVal]()
    var idx = 1
    for ((f, fIdx) <- fields.zipWithIndex) {
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
    }
    LuaTable(map)
  }

  def evalVar(v: Var, env: Env): LuaVal = v match {
    case NameVar(name) => env.get(name)
    case IndexVar(prefix, index) =>
      eval(prefix, env) match {
        case LuaTable(m) => m.getOrElse(eval(index, env), LuaNil)
        case _ => LuaNil
      }
    case DotVar(prefix, name) =>
      eval(prefix, env) match {
        case LuaTable(m) => m.getOrElse(LuaStr(name), LuaNil)
        case _ => LuaNil
      }
  }

  def setVar(v: Var, value: LuaVal, env: Env): Unit = v match {
    case NameVar(name) => env.set(name, value)
    case IndexVar(prefix, index) =>
      eval(prefix, env) match {
        case LuaTable(m) => m(eval(index, env)) = value
        case _ => throw new EvalError("tried to index something that isn't a table")
      }
    case DotVar(prefix, name) =>
      eval(prefix, env) match {
        case LuaTable(m) => m(LuaStr(name)) = value
        case _ => throw new EvalError("tried to index something that isn't a table")
      }
  }

  def evalCall(call: FunctionCall, env: Env): List[LuaVal] = {
    // println(s"evalCall: ${call}")
    val func = call.method match {
      case Some(method) =>
        eval(call.prefix, env) match {
          case LuaTable(m) => m.getOrElse(LuaStr(method), LuaNil)
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
        for ((p, i) <- params.zipWithIndex) {
          funcEnv.define(p, if (i < args.length) args(i) else LuaNil)
        }
        if (vararg) {
          val extra = args.drop(params.length)
          val vt = mutable.Map[LuaVal, LuaVal]()
          for ((v, i) <- extra.zipWithIndex) vt(LuaNum(i + 1)) = v
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
        // lua modulo is weird, not the same as % in most languages
        case "%" =>
          val a = toNum(lv)
          val b = toNum(rv)
          LuaNum(a - Math.floor(a / b) * b)
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

  def execBlock(block: Block, env: Env): Unit = {
    for (s <- block.stats) exec(s, env)
    block.retstat.foreach { ret =>
      throw new ReturnException(evalExpList(ret.explist, env))
    }
  }

  def exec(s: Stat, env: Env): Unit = s match {
    case LocalStat(names, exps) =>
      val vals = evalExpList(exps, env)
      for ((n, i) <- names.zipWithIndex) {
        env.define(n, if (i < vals.length) vals(i) else LuaNil)
      }

    case AssignStat(vars, exps) =>
      val vals = evalExpList(exps, env)
      // println(s"assign: $vars = $vals")
      for ((v, i) <- vars.zipWithIndex) {
        setVar(v, if (i < vals.length) vals(i) else LuaNil, env)
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
      val st = step.map(x => toNum(eval(x, env))).getOrElse(1.0)
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
            for ((n, i) <- names.zipWithIndex) {
              loopEnv.define(n, if (i < results.length) results(i) else LuaNil)
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
        for (n <- funcName.names.tail.dropRight(if (funcName.method.isDefined) 0 else 1)) {
          table = table match {
            case LuaTable(m) => m.getOrElse(LuaStr(n), LuaNil)
            case _ => LuaNil
          }
        }
        val lastName = funcName.method.getOrElse(funcName.names.last)
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
    case LabelStat(_) =>  // TODO: implement goto/labels
    case GotoStat(_) =>   // TODO: implement goto/labels
  }

  def isTruthy(v: LuaVal): Boolean = v match {
    case LuaNil => false
    case LuaBool(false) => false
    case _ => true
  }

  def toNum(v: LuaVal): Double = v match {
    case LuaNum(n) => n
    case LuaStr(s) => s.toDoubleOption.getOrElse(throw new EvalError(s"can't convert '$s' to number"))
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

  def isEqual(a: LuaVal, b: LuaVal): Boolean = (a, b) match {
    case (LuaNil, LuaNil) => true
    case (LuaBool(x), LuaBool(y)) => x == y
    case (LuaNum(x), LuaNum(y)) => x == y
    case (LuaStr(x), LuaStr(y)) => x == y
    case (LuaTable(x), LuaTable(y)) => x eq y  // reference equality for tables
    case _ => false
  }

  // TODO: metatables would go here

  def makeGlobalEnv(): Env = {
    val env = new Env(mutable.Map(), None)

    env.define("print", LuaBuiltin("print", args => {
      println(args.map(_.show).mkString("\t"))
      List()
    }))

    env.define("type", LuaBuiltin("type", args => {
      val t = args.headOption.getOrElse(LuaNil) match {
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
      args.headOption match {
        case Some(LuaNum(n)) => List(LuaNum(n))
        case Some(LuaStr(s)) => s.toDoubleOption.map(n => List(LuaNum(n))).getOrElse(List(LuaNil))
        case _ => List(LuaNil)
      }
    }))

    env.define("tostring", LuaBuiltin("tostring", args => {
      List(LuaStr(args.headOption.getOrElse(LuaNil).show))
    }))

    env.define("error", LuaBuiltin("error", args => {
      throw new EvalError(args.headOption.map(_.show).getOrElse("error"))
    }))

    env
  }
}