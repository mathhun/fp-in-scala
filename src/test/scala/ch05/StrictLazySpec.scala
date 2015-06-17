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

  "foldRight" should "be stream version of foldRight" in {
    Stream(1,2,3,4,5).foldRight(0)(_ + _) should be (15)
  }

  "exists" should "be true if any one of elements in stream satisfy the predicate" in {
    Stream(2,3,5,7,11).exists(_ % 2 == 0) should be (true)
    Stream(3,5,7,11).exists(_ % 2 == 0) should be (false)
  }

  it should "evaluate lazily" in {
    def pred(x: Int): Boolean = {
      println(x)
      x % 2 == 0
    }

    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      Stream(1,1,2,3,5,8).exists(pred) should be (true)
    }
    out.toString should be ("1\n1\n2\n")
  }

  "#4" should "implement forAll" in {
    Stream(2,4,6,8,10).forAll(_ % 2 == 0) should be (true)
    Stream(2,4,6,8,10).forAll(_ % 2 == 1) should be (false)
  }

  "#5" should "use foldRight to implement takeWhile" in {
    Stream(1,2,3,4,5,6).takeWhile2(_ <= 4).toList should be (List(1,2,3,4))
    Stream(10, 11, 12).takeWhile2(_ <= 5).toList should be (Nil)
  }

  "ones" should "have 1s" in {
    ones.take(5).toList should be (List(1,1,1,1,1))
  }

  "#7 constant" should "be a stream of constants" in {
    constant(5).take(5).toList should be (List(5,5,5,5,5))
    constant(10).take(5).toList should be (List(10,10,10,10,10))
  }

  "#8 from" should "be an infinite stream of integers starting from n" in {
    from(1).take(5).toList should be (List(1,2,3,4,5))
    from(5).take(5).toList should be (List(5,6,7,8,9))
  }

  "fibs" should "be fibonacci number stream" in {
    //fibs.take(10).toList should be (List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55))
  }
}
