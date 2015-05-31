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

  "#5 wordsMonoid" should "be a monoid instance that inserts spaces" in {
    List("Hic", "est ", "chorda ").foldRight(wordsMonoid.zero)(wordsMonoid.op) should be ("Hic est chorda")
  }

  "#6 concatenate" should "fold a list with a monoid" in {
    concatenate(List(1, 2, 3, 4), intAddition) should be (10)
    concatenate(List(1, 2, 3, 4), intMultiplication) should be (24)
  }

  "#7 foldMap" should "turn list elements into Monoid instances" in {
    foldMap(List("a", "bc", "def", "ghij"), intAddition)(_.length) should be (10)
    foldMap(List("a", "bc", "def", "ghij"), intMultiplication)(_.length) should be (24)
  }

  "#8" should "implement foldLeft/Right using foldMap" ignore {
    //List(0, 1, 2, 3, 4).foldRight2(intAddition.zero)(intAddition.op) should be (10)
    //List(1, 2, 3, 4, 5).foldRight2(intMultiplication.zero)(intMultiplication.op) should be (120)
  }

  "#9" should "implement wcMonoid" in {
    // TODO:
  }

  "#10" should "implement wordcount using WC" in {
    // TODO:
    //countWords("lorem ipsum dolor sit amet, ") should be (5)
  }

  "#11" should "implement an efficient foldMap for IndexedSeq" ignore {
    // TODO:
  }

  "#12" should "Use foldMap to detect whether a given IndexedSeq[Int] is ordered" ignore {
    // TODO:
  }

  "#13" should "Implement Foldable[List/IndexedSeq/Stream]" in {

  }
}
