import org.scalatest._
import com.github.merisbahti._
import scala.util.parsing.combinator._

class ParserSpec extends FlatSpec with Matchers {
  "SExpParser" should "parse symbols correctly" in {
    val symbolInputs = List("hejsan", "hej-san", "hej+san", "&hej-san")
    symbolInputs.foreach { (s: String) =>
      SExpParser.parse(SExpParser.symbol, s) match {
        case SExpParser.Success(matched,_) => assert(true)
        case SExpParser.Failure(msg,_)     => assert(false, "FAILURE: " + msg)
        case SExpParser.Error(msg,_)       => assert(false, "ERROR: " + msg)
      }
    }
  }
  "SExpParser" should "parse integers correctly" in {
    val integerInputs = List("12", "23", "34", "-12")
    val integerValues = integerInputs.map {(x: String) => Int(x.toInt)}
    integerInputs.zip(integerValues).foreach {
      case (s, value) =>
      SExpParser.parse(SExpParser.int, s) match {
        case SExpParser.Success(matched,_) => assert(matched === value)
        case SExpParser.Failure(msg,_)     => assert(false, "FAILURE: " + msg)
        case SExpParser.Error(msg,_)       => assert(false, "ERROR: " + msg)
      }
    }
  }
  "SExpParser" should "parse combinations correctly" in {
    val combInputs = List("(+ 2 3 4 5)",
      "(- 2)")
    val combValues = List(
      Comb(List(SymbolT("+"), Int(2) , Int(3), Int(4), Int(5))),
      Comb(List(SymbolT("-"), Int(2)))
    )
    combInputs.zip(combValues).foreach {
      case (s, comb) =>
      SExpParser.parse(SExpParser.comb, s) match {
        case SExpParser.Success(matched,_) => assert(matched === comb)
        case SExpParser.Failure(msg,_)     => assert(false, "FAILURE: " + msg)
        case SExpParser.Error(msg,_)       => assert(false, "ERROR: " + msg)
      }
    }
  }
  "Combs" should "be able to be nested" in {
    val input = "(+ (- 2 3) (* 4 (+ 3 5)))"
    val corrComb = Comb(List(
      SymbolT("+"),
      Comb(List(
        SymbolT("-"),
        Int(2),
        Int(3)
        )),
      Comb(List(
        SymbolT("*"),
        Int(4),
        Comb(List(
          SymbolT("+"),
          Int(3),
          Int(5)
          ))
        ))
    ))
    SExpParser.parse(SExpParser.comb, input) match {
      case SExpParser.Success(matched,_) => assert(matched === corrComb)
      case SExpParser.Failure(msg,_)     => assert(false, "FAILURE: " + msg)
      case SExpParser.Error(msg,_)       => assert(false, "ERROR: " + msg)
    }
  }
}