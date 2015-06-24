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

  def positiveInt2: Rand[Int] = {
    map(int) { i =>
      if (i != Int.MinValue) i.abs else Int.MaxValue
    }
  }

  def positiveInt3: Rand[Int] = {
    flatMap(int) { i =>
      rng => {
        if (i != Int.MinValue) (i.abs, rng) else (Int.MaxValue, rng)
      }
    }
  }

  // def double(rng: RNG): (Double, RNG) = {
  // }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  type State[S, +A] = S => (A, S)

  def map_[S, A, B](a: S => (A, S))(f: A =>B): S => (B, S) = {
    s => {
      val (a2, s2) = a(s)
      (f(a2), s2)
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      val randb = g(a)
      randb(rng2)
    }
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A =>B): S => (B, S) = {
    s => {
      val (a2, s2) = run(s)
      (f(a2), s2)
    }
  }
}
