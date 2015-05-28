package fpinscala.strictandlazy.spec
import org.scalatest._
import fpinscala.strictandlazy.StrictLazy._

class StrictLazySpec extends FlatSpec with Matchers {
  "Filter" should "" in {
    List(1, 2, 3, 4) map (_ + 10) filter (_ % 2 == 0) map (_ * 3) should be (List(36, 42))
  }

  "short-circuit evaluations" should "be lazy" in {
    (false && { println("!!"); true }) should be (false)
  }

  "if2" should "not evaluate onFalse when cond is true" in {
    val x = if2((1 == 1), "true", (throw new Exception()))
    x should be ("true")
  }

  it should "evaluate onFalse when cond is false" in {
    intercept [Exception] {
      if2((2 == 1), "true", (throw new Exception()))
    }
  }

  "pair" should "evaluate arguments each time" in {
    val stream = new java.io.ByteArrayOutputStream()
    Console.setOut(stream)

    (pair { println("hi"); 1 + 41 }) should be (42, 42)
    (stream.toString) should be ("hi\nhi\n")
  }

  "pair2" should "evaluate arguments only once" in {
    val stream = new java.io.ByteArrayOutputStream()
    Console.setOut(stream)

    (pair2 { println("hi"); 1 + 41 }) should be (42, 42)
    (stream.toString) should be ("hi\n")

  }
}
