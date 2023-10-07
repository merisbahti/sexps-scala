package com.github.merisbahti

import scala.reflect.ClassTag

import com.github.merisbahti.TypeAliases.Env
import org.scalactic.Bool

package object TypeAliases {
  type Env = Map[SymbolT, Expr]
}

object StdLib {
  def arithmeticProc(op: (Integer, Integer) => Integer) =
    Proc((xs, env) => {
      val res = evalTo[IntT](xs.head, env)
      xs.tail.foldLeft(res) {
        case ((leftExpr, env), unevaledExpr) =>
          val (rightExpr, newEnv) = evalTo[IntT](unevaledExpr, env)
          (IntT(op(leftExpr.value, rightExpr.value)), newEnv)
      }
    })

  def define = Proc { (xs: List[Expr], sEnv: Map[SymbolT, Expr]) =>
    xs match {
      case ((defs: Comb) :: List(body: Comb)) =>
        mkFunc(defs.exprs, body, sEnv)
      case ((newDef: SymbolT) :: List(expr)) =>
        val result = expr.eval(sEnv)
        (newDef, result._2 ++ Map(newDef -> result._1))
      case _ => throw new IllegalStateException("Invalid definition")
    }
  }

  def mkFunc(defs: List[Expr], body: Comb, sEnv: Map[SymbolT, Expr]) =
    defs match {
      case ((name: SymbolT) :: (vars: List[SymbolT])) =>
        (name, sEnv ++ Map(name -> Func(name, vars, body)))
      case _ => throw new IllegalStateException("Sumtin unexpected")
    }

  def display = Proc {
    case (xs, e) =>
      xs.foreach(x => println(s"${x.eval(e)._1}"))
      (NullT, e)
  }

  def evalTo[T <: Expr: ClassTag](e: Expr, env: Env): (T, Env) = {
    e.eval(env) match {
      case (x: T, e) => (x, e)
      case (v, _)    => throw new Error(s"Unexpected value: $v")
    }
  }

  def predicateProc(op: (Boolean, Boolean) => Boolean) =
    Proc((xs, env) => {
      val res = evalTo[BoolT](xs.head, env)
      xs.tail.foldLeft(res) {
        case ((leftExpr, env), unevaledExpr) =>
          val (rightExpr, newEnv) = evalTo[BoolT](unevaledExpr, env)
          (BoolT(op(leftExpr.value, rightExpr.value)), newEnv)
      }
    })

  def and = predicateProc(_ && _)
  def or  = predicateProc(_ || _)

  def plus  = arithmeticProc(_ + _)
  def minus = arithmeticProc(_ - _)
  def mul   = arithmeticProc(_ * _)
  def div   = arithmeticProc(_ / _)
  def mod   = arithmeticProc(_ % _)
  def stdLib: Map[SymbolT, Expr] = Map(
    SymbolT("+")       -> plus,
    SymbolT("-")       -> minus,
    SymbolT("*")       -> mul,
    SymbolT("/")       -> div,
    SymbolT("%")       -> mod,
    SymbolT("true")    -> BoolT(true),
    SymbolT("false")   -> BoolT(false),
    SymbolT("and")     -> and,
    SymbolT("or")      -> or,
    SymbolT("define")  -> define,
    SymbolT("display") -> display
  )
}
