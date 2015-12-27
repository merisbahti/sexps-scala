package com.github.merisbahti

import TypeAliases._

trait Expr {
  def eval(env: Env): (Expr, Env)
}

trait Value extends Expr

case class Proc(f: (List[Expr], Env) => (Expr, Env)) extends Expr{
  def apply(args: List[Expr], env: Env) = f(args, env)
  def eval(a: Env) = (this,a)
}

case class Comb (exprs: List[Expr]) extends Expr {
  def eval(env: Env) = exprs match {
    case ((a: SymbolT) :: xs) => env.get(a) match {
      case Some(func: Proc) => func.apply(xs, env)
      case None => throw new IllegalStateException(s"${a.name} is not defined in the environment.")
      case _ => throw new IllegalStateException(s"${a.name} is not a proc.")
    }
    case _ => throw new IllegalStateException("Wrong form")
  }
}

case class SymbolT(name: String) extends Expr {
  def eval(env: Env) = env.get(this) match {
      case (a: Value) => (a, env)
      case Some(x)    => x.eval(env)
      case None       => throw new IllegalStateException(s"$name is not defined in the environment")
  }
}

case class Int(value: Integer) extends Value {
  def eval(env: Env) = (Int(value), env)
}

object NullValue extends Value {
  def eval(env: Env) = (Int(1), env)
}

case class Program(exprs: List[Expr]) {
  def run() = exprs.tail.foldLeft(exprs.head.eval(StdLib.stdLib)) {
    case ((_, nEnv: Env), exp: Expr) => exp.eval(nEnv)
  }
}
