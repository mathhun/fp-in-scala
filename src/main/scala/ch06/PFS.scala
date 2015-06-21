package fpinscala.pfs

object PurelyFunctionalState {
}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  def randomPair(rng: RNG): (Int, Int) = {
    val (i1, _) = rng.nextInt
    val (i2, _) = rng.nextInt
    (i1, i2)
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i1, rng1) = rng.nextInt
    if (i1 == Int.MinValue) (Int.MaxValue, rng1)
    else (i1.abs, rng1)
  }

  //def double(rng: RNG): (Double, RNG)
}
