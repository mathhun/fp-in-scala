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

  "#1" should "implement Option.map" in {
    val f = (x: Int) => x + 13
    Option.Some(7).map(f) should be (Option.Some(20))
    Option.None.map(f) should be (Option.None)
  }

  it should "implement Option.flatMap" in {
    val f = (x: Int) => Option.Some(x + 13)
    Option.Some(7).flatMap(f) should be (Option.Some(20))
    Option.None.flatMap(f) should be (Option.None)
  }

  it should "implement Option.getOrElse" in {
    Option.Some(7).getOrElse(-1) should be (7)
    Option.None.getOrElse(-1) should be (-1)
  }

  it should "implement Option.orElse" in {
    Option.Some(7).orElse(Option.Some(13)) should be (Option.Some(7))
    Option.None.orElse(Option.Some(13)) should be (Option.Some(13))
  }

  it should "implement Option.filter" in {
    val f = (x: Int) => x >= 10
    Option.Some(7).filter(f) should be (Option.None)
    Option.Some(13).filter(f) should be (Option.Some(13))
  }

  "#2 variance" should "compute variance of Seq[Double]" in {
    variance(Seq(2, 3, 5, 7, 11)) should be (Option.Some(10.24))
    variance(Seq()) should be (Option.None)
  }
}
