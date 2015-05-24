import org.scalatest._
import MyModule._

class MyModuleSpec extends FlatSpec with Matchers {
  "abs" should "return absolute number" in {
    formatAbs(-42) should be ("The absolute value of -42 is 42")
  }

  "factorial" should "should return Nth factorial number" in {
    factorial(4) should be (24)
    factorial(10) should be (3628800)
  }
}
