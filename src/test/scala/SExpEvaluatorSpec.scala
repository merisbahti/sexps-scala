import org.scalatest._
import com.github.merisbahti._
import scala.util.parsing.combinator._

class SExpEvaluatorSpec extends FlatSpec with Matchers with BeforeAndAfter {
  var stdLib: Env = _
  before {
    val plus = {
      (xs: List[Expr], sEnv: Env) =>
        xs.foldLeft((Int(0), sEnv)) {
          case ((Int(sum), env: Env), a: Expr) =>
            a.eval(env) match {
              case (Int(value), nEnv) => (Int(sum+value), nEnv)
              case _ => throw new IllegalStateException("Not int!")
            }
          }
        }
    stdLib = Env(Map(SymbolT("+") -> plus))
  }

  "StdLib +" should "add all numbers in the form" in {
    val input = "(+ 3 2 4)"
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched,_) =>
        assert(matched.eval(stdLib)._1 === Int(9))
      case _ => assert(false, "Failed to parse test-input")
    }
  }

  "StdLib + " should "should handle nested expressions" in {
    val input = """(+ 3 2 (+ 2 3))"""
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched,_) =>
        assert(matched.eval(stdLib)._1 === Int(10))
      case _ => assert(false, "Failed to parse test-input")
    }
  }

  "StdLib + " should "should fail if all expressions except head aren't Int" in {
    val input = """(+ 3 2 +)"""
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched,_) =>
        assert(matched.eval(stdLib)._1 === Int(10))
      case _ => assert(false, "Failed to parse test-input")
    }
  }
}
