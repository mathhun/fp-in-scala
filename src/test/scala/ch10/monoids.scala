package fpinscala.monoids.spec
import org.scalatest._
import fpinscala.monoids.Monoids._

class MonoidsSpec extends FlatSpec with Matchers {
  "#1" should "give Monoid instances for integer addition/multiplication/Boolean operators" in {
    List(0, 1, 2, 3, 4).foldRight(intAddition.zero)(intAddition.op) should be (10)
    List(1, 2, 3, 4, 5).foldRight(intMultiplication.zero)(intMultiplication.op) should be (120)

    // or
    List(true, true).foldRight(booleanOr.zero)(booleanOr.op) should be (true)
    List(true, false).foldRight(booleanOr.zero)(booleanOr.op) should be (true)
    List(false, true).foldRight(booleanOr.zero)(booleanOr.op) should be (true)
    List(false, false).foldRight(booleanOr.zero)(booleanOr.op) should be (false)
    List().foldRight(booleanOr.zero)(booleanOr.op) should be (false)

    // and
    List(true, true).foldRight(booleanAnd.zero)(booleanAnd.op) should be (true)
    List(true, false).foldRight(booleanAnd.zero)(booleanAnd.op) should be (false)
    List(false, true).foldRight(booleanAnd.zero)(booleanAnd.op) should be (false)
    List(false, false).foldRight(booleanAnd.zero)(booleanAnd.op) should be (false)
    List().foldRight(booleanAnd.zero)(booleanAnd.op) should be (true)
  }

  "#2" should "give a Monoid instance for Option" in {
  }

  "#3" should "be EndoMonoid" in {
  }

  "#4" should "Need Property-based testing framework from Part 2" ignore {
  }

  "folding Monoids" should "be possible" in {
    List("Hic", "Est", "Index").foldRight(stringMonoid.zero)(stringMonoid.op) should be ("HicEstIndex")
    List("Hic", "Est", "Index").foldLeft(stringMonoid.zero)(stringMonoid.op) should be ("HicEstIndex")
  }
}
