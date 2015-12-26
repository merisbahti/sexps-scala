import org.scalatest._
import com.github.merisbahti._
import scala.util.parsing.combinator._

import TypeAliases._

class SExpEvaluatorSpec extends FlatSpec with Matchers with BeforeAndAfter {
  var stdLib: Map[SymbolT, Expr] = _
  before {
    stdLib = StdLib.stdLib
  }

  "StdLib +" should "add all numbers in the form" in {
    val input = "(+ 3 2 4)"
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched,_) =>
        assert(matched.eval(stdLib)._1 === Int(9))
      case _ => fail("Failed to parse test-input")
    }
  }

  "StdLib +" should "handle nested expressions" in {
    val input = """(+ 3 2 (+ 2 3))"""
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched,_) =>
        assert(matched.eval(stdLib)._1 === Int(10))
      case _ => fail("Failed to parse test-input")
    }
  }

  "StdLib" should "have support for +-/*%" in {
    val input = "(+ 2 (* 3 2) (/ 4 2) (% 4 2))"
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched, _) => assert(matched.eval(stdLib)._1 === Int(10))
      case _ => fail("Failed to parse test-input")
    }
  }

  "Environment" should "be able to resolve symbols which point to values" in {
    val env = StdLib.stdLib ++ Map(SymbolT("one") -> Int(1))
    val input = "(+ one one one one)"
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched, _) => assert(matched.eval(env)._1 === Int(4))
      case _ => fail("Failed to parse test-input")
    }
  }

  "StdLib +" should "fail if all expressions except head aren't Int" in {
    val input = """(+ 3 2 +)"""
    intercept[IllegalStateException] {
      SExpParser.parse(SExpParser.comb, input) match {
        case SExpParser.Success(matched,_) =>
          assert(matched.eval(stdLib)._1 === Int(10))
        case _ => fail("Failed to parse test-input")
      }
    }
  }

  "define" should "work" in {
    val input = "(define xs (* 3 2))" +
                 "(+ xs 2)"
    SExpParser.parse(SExpParser.program, input) match {
      case SExpParser.Success(matched,_) =>
        matched.foldLeft(matched.head.eval(stdLib)){
          case ((_: Expr, env: Env), e: Expr) =>
            e.eval(env)
        }
      case _ => fail("Failed to parse test-input")
    }

  }
}
