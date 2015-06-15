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

  "mkMatcher" should "return a matcher if regexp is valid, None otherwise" in {
    for {
      p <- mkMatcher("^abc")
    } yield p("abc") should be (true)

    for {
      p <- mkMatcher("invalid(")
    } yield p("abc") should be (false)
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

  "#3 map2" should "be a map for functions which take two arguments" in {
    val add = (x: Int, y: Int) => (x + y)
    map2(Some(3), Some(5))(add) should be (Some(8))
    map2(Some(3), None)(add) should be (None)
    map2(None, Some(5))(add) should be (None)
  }

  "#4" should "implement bothMatch using map2" in {
    bothMatch_2("aa*bb*", "a+b+", "aaabbb") should be (Some(true))
    bothMatch_2("cccc", "a+b+", "aaabbb") should be (Some(false))
    bothMatch_2("aa*bb*", "cccc", "aaabbb") should be (Some(false))
    bothMatch_2("^abc.*", ".*def$", "abcdef") should be (Some(true))
  }
  it should "return false when match fails" in {
    bothMatch_2("^abc.*", "xxx", "abcdef") should be (Some(false))
    bothMatch_2("xxx", ".*def$", "abcdef") should be (Some(false))
  }
  it should "return None when regexp is invalid" in {
    bothMatch_2("invalid(", "xxx", "abc") should be (None)
    bothMatch_2("xxx", "invalid(", "abc") should be (None)
  }

  "#5 sequence" should "combine List[Option[A]] into Option[List[A]]" in {
    sequence(List(Some(3), Some(5), Some(7))) should be (Some(List(3, 5, 7)))
  }
  it should "return None if the list contains None" in {
    sequence(List(Some(3), None, Some(7))) should be (None)
  }

  "#6 traverse" should "be similar as sequence.map, but traverse the list only once" in {
    sequence2(List(Some(3), Some(5), Some(7))) should be (Some(List(3, 5, 7)))
    sequence2(List(Some(3), None, Some(7))) should be (None)
  }

  "#7" should "Either.map" in {
    Right(8.0) map (_ / 2.0) should be (Right(4.0))
    Left("error") map ((x: Double) => x / 2.0) should be (Left("error"))
  }

  it should "Either.flatMap" in {
    Right(IndexedSeq(1.0, 2.0, 3.0, 5.0, 8.0)) flatMap mean_either should be (Right(3.8))
    Right(IndexedSeq()) flatMap mean_either should be (Left("mean of empty List!"))
    Left("empty") flatMap mean_either should be (Left("empty"))
  }
}
