import com.github.merisbahti._
import scala.util.parsing.combinator._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserSpec extends AnyFlatSpec with Matchers {
  "SExpParser" should "parse symbols correctly" in {
    val unused = 10

    val symbolInputs = List("hejsan", "hej-san", "hej+san", "&hej-san")
    symbolInputs.foreach { (s: String) =>
      SExpParser.parse(SExpParser.symbol, s) match {
        case SExpParser.Success(matched, _) => assert(true)
        case SExpParser.Failure(msg, _)     => fail("FAILURE: " + msg)
        case SExpParser.Error(msg, _)       => fail("ERROR: " + msg)
      }
    }
  }
  "SExpParser" should "parse integers correctly" in {
    val integerInputs = List("12", "23", "34", "-12")
    val integerValues = integerInputs.map { (x: String) => IntT(x.toInt) }
    integerInputs.zip(integerValues).foreach { case (s, value) =>
      SExpParser.parse(SExpParser.int, s) match {
        case SExpParser.Success(matched, _) => assert(matched === value)
        case SExpParser.Failure(msg, _)     => fail("FAILURE: " + msg)
        case SExpParser.Error(msg, _)       => fail("ERROR: " + msg)
      }
    }
  }
  "SExpParser" should "parse combinations correctly" in {
    val combInputs = List("(+ 2 3 4 5)", "(- 2)")
    val combValues = List(
      Comb(List(SymbolT("+"), IntT(2), IntT(3), IntT(4), IntT(5))),
      Comb(List(SymbolT("-"), IntT(2)))
    )
    combInputs.zip(combValues).foreach { case (s, comb) =>
      SExpParser.parse(SExpParser.comb, s) match {
        case SExpParser.Success(matched, _) => assert(matched === comb)
        case SExpParser.Failure(msg, _)     => fail("FAILURE: " + msg)
        case SExpParser.Error(msg, _)       => fail("ERROR: " + msg)
      }
    }
  }
  "Combs" should "be able to be nested" in {
    val input = "(+ (- 2 3) (* 4 (+ 3 5)))"
    val corrComb = Comb(
      List(
        SymbolT("+"),
        Comb(
          List(
            SymbolT("-"),
            IntT(2),
            IntT(3)
          )
        ),
        Comb(
          List(
            SymbolT("*"),
            IntT(4),
            Comb(
              List(
                SymbolT("+"),
                IntT(3),
                IntT(5)
              )
            )
          )
        )
      )
    )
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched, _) => assert(matched === corrComb)
      case SExpParser.Failure(msg, _)     => fail("FAILURE: " + msg)
      case SExpParser.Error(msg, _)       => fail("ERROR: " + msg)
    }
  }
}
