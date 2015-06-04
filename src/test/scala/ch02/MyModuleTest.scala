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

  "#1 fib" should "compute Nth fibonacci number" in {
    Range(0,10).map(fib) should be (Vector(0,1,1,2,3,5,8,13,21,34))
    fib(30) should be (832040)
  }

  "#2 isSorted" should "checks whether an Array[A] is sorted" in {
    isSorted(Array(0,1,2,3,4), (x: Int, y: Int) => x > y) should be (true)
    isSorted(Array(1,2,0,3,4), (x: Int, y: Int) => x > y) should be (false)
  }

  "#3 partial1" should "be a higher-order function doing partial application" in {
    val f = partial1(13, (x: Int, y: Int) => x + y)
    f(0) should be (13)
    f(1) should be (14)
    f(2) should be (15)
  }
}
