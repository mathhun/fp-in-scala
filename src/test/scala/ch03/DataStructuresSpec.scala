package fpinscala.datastructures.spec
import org.scalatest._
import fpinscala.datastructures._

class DataStructuresSpec extends FlatSpec with Matchers {
  "List" should "be of type fpinscala.datastructures.List" in {
    assert(List(1).isInstanceOf[fpinscala.datastructures.List[Int]])
  }

  "List.sum" should "calculate the sum of numbers" in {
    List.sum(List(1, 2, 3, 4)) should be (10)
    List.sum(Cons(1, Cons(2, Cons(3, Nil)))) should be (6)
  }

  "List.product" should "calculate the product of numbers" in {
    List.product(List(1, 2, 3, 4)) should be (24)
    List.product(Cons(1, Cons(2, Cons(3, Nil)))) should be (6)
  }

  "List.sum2" should "calculate the sum of numbers" in {
    List.sum2(List(1, 2, 3, 4)) should be (10)
    List.sum2(Cons(1, Cons(2, Cons(3, Nil)))) should be (6)
  }

  "List.product2" should "calculate the product of numbers" in {
    List.product2(List(1, 2, 3, 4)) should be (24.0)
    List.product2(Cons(1, Cons(2, Cons(3, Nil)))) should be (6.0)
  }

  "List.sum3" should "calculate the sum of numbers" in {
    List.sum3(List(1, 2, 3, 4)) should be (10)
    List.sum3(Cons(1, Cons(2, Cons(3, Nil)))) should be (6)
  }

  "List.product3" should "calculate the product of numbers" in {
    List.product3(List(1, 2, 3, 4)) should be (24.0)
    List.product3(Cons(1, Cons(2, Cons(3, Nil)))) should be (6.0)
  }

  "Exercise 1" should "match 'x + y'" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    x should be (3)
  }

  "tail" should "return the list without the first element" in {
    List.tail(List(1, 2, 3)) should be (List(2, 3))
  }

  "drop" should "return the list without the first n elements" in {
    List.drop(List(1, 2, 3, 4, 5), 0) should be (List(1, 2, 3, 4, 5))
    List.drop(List(1, 2, 3, 4, 5), 1) should be (List(2, 3, 4, 5))
    List.drop(List(1, 2, 3, 4, 5), 2) should be (List(3, 4, 5))
  }

  "dropWhile" should "remove elements as long as the predicate returns true" in {
    List.dropWhile(List(-3, -2, -1, 0, 1, 2, 3))(_ <= 0) should be (List(1, 2, 3))
  }

  "setHead" should "replace the head element" in {
    List.setHead(List(1, 2, 3), 100) should be (List(100, 2, 3))
  }

  "append" should "concatenate two lists" in {
    List.append(List(1, 2, 3), List(4, 5, 6)) should be (List(1, 2, 3, 4, 5, 6))
  }

  "append2" should "concatenate two lists" in {
    List.append2(List(1, 2, 3), List(4, 5, 6)) should be (List(1, 2, 3, 4, 5, 6))
  }

  "init" should "return a List without the last element" in {
    List.init(List(1, 2, 3)) should be (List(1, 2))
  }

  "foldRight w/ cons" should "clone the list" in {
    List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) should be (List(1, 2, 3))
  }

  "length" should "compute the list length" in {
    assert(List.length(List()) == 0)
    assert(List.length(List(1, 2, 3)) == 3)
    assert(List.length(List(1, 2, 3, 4, 5)) == 5)
  }

  "length2" should "compute the list length" in {
    assert(List.length2(List()) == 0)
    assert(List.length2(List(1, 2, 3)) == 3)
    assert(List.length2(List(1, 2, 3, 4, 5)) == 5)
  }

  "foldRight" should "throw StackOverflowError" in {
    var x = Nil: List[Int]
    for (i <- 0 to 10000) {
      x = Cons(i, x)
    }

    intercept[StackOverflowError] {
      List.length(x)
    }
  }

  "reverse" should "return the reversed list" in {
    List.reverse(List()) should be (List())
    List.reverse(List(1, 2, 3)) should be (List(3, 2, 1))
  }

  "#15 flatten" should "return the concatenated list" in {
    List.flatten(List(List(1), List(2), List(3))) should be (List(1, 2, 3))
    List.flatten(List(List(1, 2), List(3, 4), List(5, 6))) should be (List(1, 2, 3, 4, 5, 6))
  }

  it should "flatten all the way down" in {
    //List.flatten(List(List(List(1)), List(2, 3), List(List(4, List(5))))) should be (List(1, 2, 3, 4, 5))
  }
}
