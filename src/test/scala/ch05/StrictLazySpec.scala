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

  "#1 toList" should "convert a stream to a list" in {
    Stream(1, 2, 3, 4).toList should be (List(1, 2, 3, 4))
    Stream().toList should be (List())
  }

  "#2 take" should "return first N element in stream" in {
    Stream(1,2,3,4,5).take(2).toList should be (Stream(1,2).toList)
  }

  "#3 takeWhile" should "return the first elements while they satisfy the predicate" in {
    Stream(1,2,3,4,5,6).takeWhile(_ <= 4).toList should be (List(1,2,3,4))
    Stream(10, 11, 12).takeWhile(_ <= 5).toList should be (Nil)
  }
}
