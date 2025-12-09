import scala.collection.mutable

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

class Env(var vars: mutable.Map[String, LuaVal], val parent: Option[Env]) {

  def get(name: String): LuaVal = {
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
      case None => vars(name) = value
    }
  }

  def define(name: String, value: LuaVal): Unit = {
    vars(name) = value
  }

  def child(): Env = new Env(mutable.Map(), Some(this))
}

object Eval {

  def eval(e: Exp, env: Env): LuaVal = e match {
    case NilExp() => LuaNil
    case TrueExp() => LuaBool(true)
    case FalseExp() => LuaBool(false)
    case Numeral(s) => LuaNum(s.toDouble)
    case LiteralString(s) => LuaStr(s)
    case ParenExp(inner) => eval(inner, env)
    case UnopExp(op, exp) => evalUnop(op, eval(exp, env))
    case BinopExp(l, op, r) => evalBinop(l, op, r, env)
    case VarExp(v) => evalVar(v, env)
    case TableConstructor(fields) => evalTable(fields, env)
    case FunctionDefExp(body) => LuaFunc(body.params, body.vararg, body.block, env)
    case FunctionCallExp(call) =>
      val results = evalCall(call, env)
      if (results.isEmpty) LuaNil else results.head
    case VarargExp() =>
      env.get("...") match {
        case LuaTable(m) => m.getOrElse(LuaNum(1), LuaNil)
        case _ => LuaNil
      }
  }

  def evalTable(fields: List[Field], env: Env): LuaTable = {
    val map = mutable.Map[LuaVal, LuaVal]()
    var idx = 1
    for (f <- fields) {
      f match {
        case ExpKeyField(k, v) => map(eval(k, env)) = eval(v, env)
        case NameKeyField(n, v) => map(LuaStr(n)) = eval(v, env)
        case ValueField(v) =>
          map(LuaNum(idx)) = eval(v, env)
          idx += 1
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
        case _ => throw new EvalError("cannot index non-table")
      }
    case DotVar(prefix, name) =>
      eval(prefix, env) match {
        case LuaTable(m) => m(LuaStr(name)) = value
        case _ => throw new EvalError("cannot index non-table")
      }
  }

  def evalCall(call: FunctionCall, env: Env): List[LuaVal] = {
    val func = call.method match {
      case Some(method) =>
        eval(call.prefix, env) match {
          case LuaTable(m) => m.getOrElse(LuaStr(method), LuaNil)
          case _ => LuaNil
        }
      case None => eval(call.prefix, env)
    }

    val args = call.method match {
      case Some(_) => eval(call.prefix, env) :: call.args.map(a => eval(a, env))
      case None => call.args.map(a => eval(a, env))
    }

    func match {
      case LuaFunc(params, vararg, body, closure) =>
        val funcEnv = closure.child()
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
      case _ => throw new EvalError("attempt to call non-function")
    }
  }

  def evalUnop(op: String, v: LuaVal): LuaVal = op match {
    case "-" => LuaNum(-toNum(v))
    case "not" => LuaBool(!isTruthy(v))
    case "#" => v match {
      case LuaStr(s) => LuaNum(s.length)
      case LuaTable(m) =>
        var len = 0
        while (m.contains(LuaNum(len + 1))) len += 1
        LuaNum(len)
      case _ => throw new EvalError("# requires string or table")
    }
    case "~" => LuaNum(~toNum(v).toLong)
    case _ => throw new EvalError(s"unknown unary op: $op")
  }

  def evalBinop(l: Exp, op: String, r: Exp, env: Env): LuaVal = op match {
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
        case "%" => LuaNum(toNum(lv) % toNum(rv))
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

  def exec(s: Stat, env: Env): Unit = s match {
    case LocalStat(names, exps) =>
      val vals = exps.map(e => eval(e, env))
      for ((n, i) <- names.zipWithIndex) {
        env.define(n, if (i < vals.length) vals(i) else LuaNil)
      }

    case AssignStat(vars, exps) =>
      val vals = exps.map(e => eval(e, env))
      for ((v, i) <- vars.zipWithIndex) {
        setVar(v, if (i < vals.length) vals(i) else LuaNil, env)
      }

    case FunctionCallStat(call) => evalCall(call, env)

    case IfStat(cond, thenBlock, elseifs, elseBlock) =>
      if (isTruthy(eval(cond, env))) {
        execBlock(thenBlock, env.child())
      } else {
        var done = false
        for (elif <- elseifs if !done) {
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
          if (isTruthy(eval(cond, loopEnv))) cont = false
        }
      } catch { case _: BreakException => }

    case ForNumStat(name, start, end, step, block) =>
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
    case LabelStat(_) =>
    case GotoStat(_) =>
  }

  def execBlock(block: Block, env: Env): Unit = {
    for (s <- block.stats) exec(s, env)
    block.retstat.foreach { ret =>
      throw new ReturnException(ret.explist.map(e => eval(e, env)))
    }
  }

  def isTruthy(v: LuaVal): Boolean = v match {
    case LuaNil => false
    case LuaBool(false) => false
    case _ => true
  }

  def toNum(v: LuaVal): Double = v match {
    case LuaNum(n) => n
    case LuaStr(s) => s.toDoubleOption.getOrElse(throw new EvalError(s"cannot convert '$s' to number"))
    case _ => throw new EvalError("expected number")
  }

  def toStr(v: LuaVal): String = v match {
    case LuaStr(s) => s
    case LuaNum(n) => if (n == n.toLong) n.toLong.toString else n.toString
    case _ => throw new EvalError("expected string or number")
  }

  def compare(a: LuaVal, b: LuaVal): Int = (a, b) match {
    case (LuaNum(x), LuaNum(y)) => x.compare(y)
    case (LuaStr(x), LuaStr(y)) => x.compare(y)
    case _ => throw new EvalError("cannot compare these types")
  }

  def isEqual(a: LuaVal, b: LuaVal): Boolean = (a, b) match {
    case (LuaNil, LuaNil) => true
    case (LuaBool(x), LuaBool(y)) => x == y
    case (LuaNum(x), LuaNum(y)) => x == y
    case (LuaStr(x), LuaStr(y)) => x == y
    case (LuaTable(x), LuaTable(y)) => x eq y
    case _ => false
  }

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

    env.define("assert", LuaBuiltin("assert", args => {
      if (!isTruthy(args.headOption.getOrElse(LuaNil))) {
        throw new EvalError(args.lift(1).map(_.show).getOrElse("assertion failed"))
      }
      args
    }))

    env.define("pairs", LuaBuiltin("pairs", args => {
      args.headOption match {
        case Some(t @ LuaTable(m)) =>
          val keys = m.keys.toList
          var idx = 0
          val iter = LuaBuiltin("pairs_iter", _ => {
            if (idx >= keys.length) List(LuaNil)
            else {
              val k = keys(idx)
              idx += 1
              List(k, m(k))
            }
          })
          List(iter, t, LuaNil)
        case _ => List(LuaNil, LuaNil, LuaNil)
      }
    }))

    env.define("ipairs", LuaBuiltin("ipairs", args => {
      args.headOption match {
        case Some(t @ LuaTable(m)) =>
          var idx = 0
          val iter = LuaBuiltin("ipairs_iter", _ => {
            idx += 1
            m.get(LuaNum(idx)) match {
              case Some(v) => List(LuaNum(idx), v)
              case None => List(LuaNil)
            }
          })
          List(iter, t, LuaNum(0))
        case _ => List(LuaNil, LuaNil, LuaNil)
      }
    }))

    env.define("next", LuaBuiltin("next", args => {
      (args.headOption, args.lift(1)) match {
        case (Some(LuaTable(m)), keyOpt) =>
          val keys = m.keys.toList
          val key = keyOpt.getOrElse(LuaNil)
          if (key == LuaNil) {
            if (keys.isEmpty) List(LuaNil)
            else List(keys.head, m(keys.head))
          } else {
            val i = keys.indexOf(key)
            if (i < 0 || i + 1 >= keys.length) List(LuaNil)
            else List(keys(i + 1), m(keys(i + 1)))
          }
        case _ => List(LuaNil)
      }
    }))

    env.define("select", LuaBuiltin("select", args => {
      args.headOption match {
        case Some(LuaStr("#")) => List(LuaNum(args.length - 1))
        case Some(LuaNum(n)) => if (n.toInt > 0) args.drop(n.toInt) else args.takeRight(-n.toInt)
        case _ => List()
      }
    }))

    env.define("pcall", LuaBuiltin("pcall", args => {
      args.headOption match {
        case Some(f @ (LuaFunc(_, _, _, _) | LuaBuiltin(_, _))) =>
          try {
            val results = f match {
              case LuaBuiltin(_, fn) => fn(args.tail)
              case LuaFunc(params, _, body, closure) =>
                val funcEnv = closure.child()
                for ((p, i) <- params.zipWithIndex) {
                  funcEnv.define(p, args.tail.lift(i).getOrElse(LuaNil))
                }
                try { execBlock(body, funcEnv); List() }
                catch { case r: ReturnException => r.values }
              case _ => List()
            }
            LuaBool(true) :: results
          } catch {
            case e: Exception => List(LuaBool(false), LuaStr(e.getMessage))
          }
        case _ => List(LuaBool(false), LuaStr("not a function"))
      }
    }))

    env.define("rawget", LuaBuiltin("rawget", args => {
      (args.headOption, args.lift(1)) match {
        case (Some(LuaTable(m)), Some(k)) => List(m.getOrElse(k, LuaNil))
        case _ => List(LuaNil)
      }
    }))

    env.define("rawset", LuaBuiltin("rawset", args => {
      (args.headOption, args.lift(1), args.lift(2)) match {
        case (Some(t @ LuaTable(m)), Some(k), Some(v)) => m(k) = v; List(t)
        case _ => List(LuaNil)
      }
    }))

    env.define("setmetatable", LuaBuiltin("setmetatable", args => {
      List(args.headOption.getOrElse(LuaNil))
    }))

    env.define("getmetatable", LuaBuiltin("getmetatable", _ => List(LuaNil)))

    env
  }
}
