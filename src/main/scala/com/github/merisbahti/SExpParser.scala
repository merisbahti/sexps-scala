package com.github.merisbahti

import scala.util.parsing.combinator._

object SExpParser extends RegexParsers {
  def symbol: Parser[SymbolT]  = """[-+*/!@%^&=.a-zA-Z0-9_]+""".r ^^ { (x: String) => SymbolT(x.toString) }
  def int: Parser[IntT]        = """-?\d+""".r ^^ { x => IntT(x.toInt) }
  def expr: Parser[Expr]       = int | symbol | comb
  def comb: Parser[Comb]       = "(" ~> rep(expr) <~ ")" ^^ { Comb(_) }
  def program: Parser[Program] = rep(expr) ^^ { Program(_) }
}
