package com.github.merisbahti

package object TypeAliases {
    type Env = Map[SymbolT, Expr]
}

object StdLib {
  def arithmeticProc(op: (Integer, Integer) => (Integer)) = Proc({
    (xs: List[Expr], sEnv: Map[SymbolT, Expr]) =>
      xs.head.eval(sEnv) match {
        case (a: IntT, e: Map[SymbolT, Expr]) =>
          xs.tail.foldLeft(xs.head.eval(sEnv)) {
            case ((IntT(sum), env: Map[SymbolT, Expr]), a: Expr) =>
              a.eval(env) match {
                case (IntT(value), nEnv) => (IntT(op(sum,value)), nEnv)
                case _ => throw new ArithmeticException("Not int found in arit: 2")
              }
          }
        case _ => throw new ArithmeticException("Not int found in arit: 1")
      }
  })

  // (define abc (2))
  // (define (abc a1 a2) (expr/
  def define = Proc({
    (xs: List[Expr], sEnv: Map[SymbolT, Expr]) =>
      xs match {
        case ((defs: Comb):: List(body:Comb)) =>
          mkFunc(defs.exprs, body, sEnv)
        case ((newDef: SymbolT) :: List(expr)) => (newDef, sEnv ++ Map(newDef -> expr))
        case _ => throw new IllegalStateException("2: NonoNo")
      }
  })

  def mkFunc(defs: List[Expr], body: Comb, sEnv: Map[SymbolT, Expr]) =
    defs match {
      case((name: SymbolT) :: (vars: List[SymbolT])) =>
        (name, sEnv ++ Map(name -> Func(name, vars, body)))
      case _ => throw new IllegalStateException("Sumtin unexpected")
    }

  def display = Proc({
    case (x, e: Map[SymbolT, Expr]) =>
      println(s"$x")
      (NullT, e)
  })

  def predicateProc(op: (Boolean, Boolean) => (Boolean)) = Proc({
    (xs: List[Expr], sEnv: Map[SymbolT, Expr]) =>
      xs.head.eval(sEnv) match {
        case (a: BoolT, e: Map[SymbolT, Expr]) =>
          xs.tail.foldLeft(xs.head.eval(sEnv)) {
            case ((BoolT(sum), env: Map[SymbolT, Expr]), a: Expr) =>
              a.eval(env) match {
                case (BoolT(value), nEnv) => (BoolT(op(sum,value)), nEnv)
                case _ => throw new ArithmeticException("Not bool found in pred: 2")
              }
          }
        case _ =>
          throw new ArithmeticException(s"Not bool found in pred: 1 ${xs.head} = ${xs.head.eval(sEnv)}")
      }
  })

  def and   = predicateProc(_&&_)
  def or    = predicateProc(_||_)

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
    SymbolT("true") -> BoolT(true),
    SymbolT("false") -> BoolT(false),
    SymbolT("and") -> and,
    SymbolT("or") -> or,
    SymbolT("define") -> define,
    SymbolT("display") -> display
    )
}
