package com.github.merisbahti

case class Env(env: Map[SymbolT,((List[Expr], Env) => (Expr, Env))]) {
  def get(s: SymbolT) = env.get(s)
}

trait Expr {
  def eval(env: Env): (Expr, Env)
}
trait Value extends Expr

case class Comb (exprs: List[Expr]) extends Expr {
  def eval(env: Env) = exprs match {
    case ((a: SymbolT) :: xs) => env.get(a) match {
      case Some(func) => func(xs, env)
      case None => throw new IllegalStateException(s"No such function: ${a.name}")
    }
    case _ => throw new IllegalStateException("Wrong form")
  }
}
case class SymbolT(name: String) extends Expr {
  def eval(env: Env) = (this, env)
}


case class Int(value: Integer) extends Value {
  def eval(env: Env) = (Int(value), env)
}

object NullValue extends Value {
  def eval(env: Env) = (Int(1), env)
}
