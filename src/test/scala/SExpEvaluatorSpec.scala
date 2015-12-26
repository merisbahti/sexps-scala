import org.scalatest._
import com.github.merisbahti._
import scala.util.parsing.combinator._

class SExpEvaluatorSpec extends FlatSpec with Matchers with BeforeAndAfter {
  var stdLib: Env = _
  before {
   val plus = Proc({
     (xs: List[Expr], sEnv: Env) =>
       xs.foldLeft((Int(0), sEnv)) {
         case ((Int(sum), env: Env), a: Expr) =>
           a.eval(env) match {
             case (Int(value), nEnv) => (Int(sum+value), nEnv)
             case _ => throw new IllegalStateException("Not int!")
           }
         }
       })
    val one = Int(1)
    stdLib = Env(Map(SymbolT("+") -> plus, SymbolT("one") -> one))
  }

  "StdLib +" should "add all numbers in the form" in {
    val input = "(+ 3 2 4)"
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched,_) =>
        assert(matched.eval(stdLib)._1 === Int(9))
      case _ => assert(false, "Failed to parse test-input")
    }
  }

  "StdLib + " should "handle nested expressions" in {
    val input = """(+ 3 2 (+ 2 3))"""
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched,_) =>
        assert(matched.eval(stdLib)._1 === Int(10))
      case _ => assert(false, "Failed to parse test-input")
    }
  }

  "Environment" should "be able to resolve symbols which point to values" in {
    val input = "(+ one one one one)"
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched, _) => assert(matched.eval(stdLib)._1 === Int(4))
      case _ => assert(false, "Failed to parse test-input")
    }
  }

  "StdLib + " should "fail if all expressions except head aren't Int" in {
    val input = """(+ 3 2 +)"""
    intercept[IllegalStateException] {
      SExpParser.parse(SExpParser.comb, input) match {
        case SExpParser.Success(matched,_) =>
          assert(matched.eval(stdLib)._1 === Int(10))
        case _ => assert(false, "Failed to parse test-input")
      }
    }
  }
}
