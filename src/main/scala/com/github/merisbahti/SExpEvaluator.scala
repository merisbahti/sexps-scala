package com.github.merisbahti

import TypeAliases._

sealed trait Expr {
  def eval(env: Env): (Expr, Env)
}

//trait Value extends Expr

trait Applyable {
  def apply(args: List[Expr], env: Env): (Expr, Env)
}

case class Proc(f: (List[Expr], Env) => (Expr, Env)) extends Expr with Applyable {
  def apply(args: List[Expr], env: Env) = f(args, env)
  def eval(a: Env)                      = (this, a)
}

case class Func(name: SymbolT, vars: List[SymbolT], body: Comb) extends Expr with Applyable {
  def apply(args: List[Expr], env: Env): (Expr, Env) = {
    val binds: List[(Expr, Env)]  = args.map(_.eval(env))
    val nEnv: Env                 = binds.reverse.head._2
    val evaluatedArgs: List[Expr] = binds.map(_._1)
    (body.eval(vars.zip(evaluatedArgs).toMap ++ nEnv)._1, nEnv)
  }
  def eval(env: Env) = ???
}

case class Comb(exprs: List[Expr]) extends Expr {
  def eval(env: Env) = exprs match {
    case ((a: SymbolT) :: xs) =>
      env.get(a) match {
        case Some(a: Applyable) => a.apply(xs, env)
        case Some(_: Expr)      => (this, env)
        case None =>
          throw new IllegalStateException(
            s"${a.name} is not defined in the environment."
          )
      }
    case _ => (this, env)
  }
}

case class SymbolT(name: String) extends Expr {
  def eval(env: Env) = env.get(this) match {
    case (a: Value) => (a, env)
    case Some(x)    => x.eval(env)
    case None       => (NullT, env)
  }
}

trait Value extends Expr {
  def eval(env: Env) = (this, env)
}

case class IntT(value: Integer)  extends Value
case class BoolT(value: Boolean) extends Value
object NullT                     extends Value

case class Program(exprs: List[Expr]) {
  def run(env: Env) = exprs.tail.foldLeft(exprs.head.eval(env)) {
    case ((_, nEnv: Env), exp: Expr) => exp.eval(nEnv)
  }
}
