package fpinscala.pfs.spec
import org.scalatest._
import fpinscala.pfs._

class PurelyFunctionalStateSpec extends FlatSpec with Matchers {
  "java.util.Random" should "return a positive Double" in {
    val rng = new java.util.Random
    rng.nextDouble should be >= 0.0
  }

  val rng: RNG = RNG.simple(1)

  "randomPair" should "return a pair of same Ints" in {
    val x = RNG.randomPair(rng)
    x._1 == x._2 should be (true)
  }

  "#1 positiveInt" should "return a positive int" in {
    RNG.positiveInt(rng)._1 should be >= 0
  }

  "#2 double" should "return a Double [0, 1)" in {
  }

  "#5 positiveMax" should "" in {
  }

  "#9" should "reimplement positiveInt using flatMap" in {
    RNG.positiveInt3(rng)._1 should be >= 0
  }
}
