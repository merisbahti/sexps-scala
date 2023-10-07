import com.github.merisbahti._
import scala.util.parsing.combinator._

import TypeAliases._
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec

class SExpEvaluatorSpec extends AnyFlatSpec with Matchers with BeforeAndAfter {
  val stdLib: Map[SymbolT, Expr] = StdLib.stdLib

  "StdLib +" should "add all numbers in the form" in {
    val input = "(+ 3 2 4)"
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched, _) =>
        assert(matched.eval(stdLib)._1 === IntT(9))
      case _ => fail("Failed to parse test-input")
    }
  }

  "StdLib +" should "handle nested expressions" in {
    val input = """(+ 3 2 (+ 2 3))"""
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched, _) =>
        assert(matched.eval(stdLib)._1 === IntT(10))
      case _ => fail("Failed to parse test-input")
    }
  }

  "StdLib" should "have support for +-/*%" in {
    val input = "(+ 2 (* 3 2) (/ 4 2) (% 4 2))"
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched, _) =>
        assert(matched.eval(stdLib)._1 === IntT(10))
      case _ => fail("Failed to parse test-input")
    }
  }

  "StdLib" should "have support for and & or" in {
    val inputs = List(
      "(and false true)",
      "(or  false true)",
      "(and true true)",
      "(or  false false)",
      "(and true false)",
      "(or  true false)",
      "(and false false)",
      "(or  true true)"
    )
    val answers = List(
      false, true, true, false, false, true, false, true
    )

    inputs.zip(answers).foreach { case ((input: String, answer: Boolean)) =>
      SExpParser.parse(SExpParser.comb, input) match {
        case SExpParser.Success(matched, _) =>
          assert(matched.eval(stdLib)._1 === BoolT(answer))
        case _ => fail("Failed to parse test-input")
      }
    }
  }

  "StdLib" should "have support for `and failure " in {

    SExpParser.parse(SExpParser.comb, "(and 1 true)") match {
      case SExpParser.Success(matched, _) =>
        assertThrows[Error](matched.eval(stdLib)._1)
      case _ => fail("Failed to parse test-input")
    }

  }

  "Environment" should "be able to resolve symbols which point to values" in {
    val env = StdLib.stdLib ++ Map(SymbolT("one") -> IntT(1))
    val input = "(+ one one one one)"
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched, _) =>
        assert(matched.eval(env)._1 === IntT(4))
      case _ => fail("Failed to parse test-input")
    }
  }

  "StdLib +" should "fail if all expressions except head aren't Int" in {
    val inputs = List("(+ 3 2 +)", "(+ (display 3) 2)")
    inputs.foreach { (input: String) =>
      intercept[Error] {
        SExpParser.parse(SExpParser.comb, input) match {
          case SExpParser.Success(matched, _) => matched.eval(StdLib.stdLib)
          case _ => fail("Failed to parse test-input")
        }
      }
    }

  }

  "define" should "work" in {
    val input = "(define xs (* 3 2))" +
      "(+ xs 2)"
    SExpParser.parse(SExpParser.program, input) match {
      case SExpParser.Success(matched, _) =>
        matched.exprs.foldLeft(matched.exprs.head.eval(stdLib)) {
          case ((_: Expr, env: Env), e: Expr) =>
            e.eval(env)
        }
      case _ => fail("Failed to parse test-input")
    }
  }

  "display" should "work" in {
    val input = "(display 10)"
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched, _) => matched.eval(stdLib)
      case _                              => fail("Failed to parse test-input")
    }
  }

}
