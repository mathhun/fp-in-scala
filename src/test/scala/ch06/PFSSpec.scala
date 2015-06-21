package fpinscala.pfs.spec
import org.scalatest._
import fpinscala.pfs._

class PurelyFunctionalStateSpec extends FlatSpec with Matchers {
  // "java.util.Random" should "" in {
  //   val rng = new java.util.Random
  //   rng.nextDouble should be >= 0
  // }

  val rng: RNG = RNG.simple(1)

  "randomPair" should "return a pair of same Ints" in {
    val x = RNG.randomPair(rng)
    x._1 == x._2 should be (true)
  }

  "#1 positiveInt" should "return a positive int" in {
    RNG.positiveInt(rng)._1 should be >= 0
  }
}
