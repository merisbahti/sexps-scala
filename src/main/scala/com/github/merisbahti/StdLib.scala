package com.github.merisbahti

package object TypeAliases {
    type Env = Map[SymbolT, Expr]
}

object StdLib {
  def arithmeticProc(op: ((Integer, Integer) => (Integer))) = Proc({
    (xs: List[Expr], sEnv: Map[SymbolT, Expr]) =>
      xs.head.eval(sEnv) match {
        case ((a: Int), (e: Map[SymbolT, Expr])) =>
          xs.tail.foldLeft(xs.head.eval(sEnv)) {
            case ((Int(sum), env: Map[SymbolT, Expr]), a: Expr) =>
              a.eval(env) match {
                case (Int(value), nEnv) => (Int(op(sum,value)), nEnv)
                case _ => throw new ArithmeticException("Not int found in arit")
              }
          }
        case _ => throw new ArithmeticException("Not int found in arit")
      }
  })

  // (define abc (2))
  def define = Proc({
    (xs: List[Expr], sEnv: Map[SymbolT, Expr]) =>
      xs match {
        case ((newDef:SymbolT) :: List(expr)) => (newDef, sEnv ++ Map(newDef -> expr))
        case _ => throw new IllegalStateException("2: NonoNo")
      }
  })

  def display = Proc({
    case (x, e: Map[SymbolT, Expr]) =>
      println(s"$x")
      (NullValue, e)
  })

  def plus  = arithmeticProc(_+_)
  def minus = arithmeticProc(_-_)
  def mul   = arithmeticProc(_*_)
  def div   = arithmeticProc(_/_)
  def mod   = arithmeticProc(_%_)
  def stdLib: Map[SymbolT, Expr] = Map(
    SymbolT("+") -> plus,
    SymbolT("-") -> minus,
    SymbolT("*") -> mul,
    SymbolT("/") -> div,
    SymbolT("%") -> mod,
    SymbolT("define") -> define,
    SymbolT("display") -> display
    )
}
