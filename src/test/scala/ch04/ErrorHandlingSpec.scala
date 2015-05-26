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

  "map" should "" in {
    (Some(2.0).map(_ * 3.0)) should be (Some(6.0))
  }
}
