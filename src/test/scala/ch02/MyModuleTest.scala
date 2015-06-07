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

  def add(a: Int, b: Int): Int = a + b
  val add10 = partial1(10, add)
  val cadd = curry(add)
  val uadd = uncurry(cadd)
  
  "#3 partial1" should "be a higher-order function doing partial application" in {
    val f = partial1(13, (x: Int, y: Int) => x + y)
    f(0) should be (13)
    f(1) should be (14)
    f(2) should be (15)

    add10(0) should be (10)
    add10(2) should be (12)
  }

  "#4 curry" should "converts a function of N arguments into a function of one argument" in {
    cadd(1)(2) should be (3)
  }

  "#5 uncurry" should "" in {
    uadd(2, 3) should be (5)
  }

  "#6 compose" should "be a higher-order function which composes two functions" in {
    def pow2(a: Double): Double = math.pow(a, 2)
    def div2(a: Double): Double = a / 2
    compose(div2, pow2)(5) should be (12.5)
  }
}
