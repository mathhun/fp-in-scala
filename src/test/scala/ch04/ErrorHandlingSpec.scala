package fpinscala.errorhandling.spec
import org.scalatest._
import fpinscala.errorhandling.ErrorHandling._
import fpinscala.errorhandling._

class ErrorHandlingSpec extends FlatSpec with Matchers {
  "mean" should "calculate the mean of a list" in {
    mean(List(1, 2, 3)) should be (2)
  }
  it should "throw an exception for empty input" in {
    intercept [ArithmeticException] {
      mean(List())
    }
  }

  "mean_1" should "return the default value given" in {
    mean_1(IndexedSeq(), 123.45) should be (123.45)
  }

  "mean" should "return Some value when valid" in {
    Option.mean(List(1, 2, 3)) should be (Option.Some(2))
  }

  it should "return None value when invalid" in {
    Option.mean(List()) should be (Option.None)
  }

  "map" should "apply a function to Some(x) value" in {
    (Some(2.0).map(_ * 3.0)) should be (Some(6.0))
  }

  "pattern" should "return None when PatternSyntaxException" in {
    pattern("regexp(") should be (None)
  }

  "doesMatch" should "return Some(true) when matched" in {
    doesMatch("regexp?", "regex") should be (Some(true))
    doesMatch("regexp?", "regexp") should be (Some(true))
    doesMatch("regexp?", "doesntmatch") should be (Some(false))
  }

  "bothMatch" should "return Some(true) when matched" in {
    bothMatch("aa*bb*", "a+b+", "aaabbb") should be (Some(true))
    bothMatch("cccc", "a+b+", "aaabbb") should be (Some(false))
    bothMatch("aa*bb*", "cccc", "aaabbb") should be (Some(false))
  }

  "mean_either" should "return Right(the mean of doubles)" in {
    mean_either(IndexedSeq(1.0, 2.0, 3.0, 4.0, 5.0)) should be (Right(3.0))
  }
  it should "return Left value when the list is empty" in {
    mean_either(IndexedSeq()) should be (Left("mean of empty List!"))
  }

  "safeDiv" should "Right" in {
    safeDiv(5.0, 2.0) should be (Right(2.5))
  }

  it should "Left?" in {
    safeDiv(5.0, 0.0) should be (Right(1.0/0.0))
  }
}
