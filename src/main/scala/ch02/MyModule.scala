object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(m: Int, a: Int, b: Int): Int =
      if (m == 0) a
      else go(m - 1, b, a + b)
    go(n, 0, 1)
  }

  def binarySearch[A](as: Array[A], key: A, gt: (A, A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key, a)) mid2
        else if (greater) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int): Boolean = {
      if (i == 0) gt(as(1), as(0))
      else if (gt(as(i + 1), as(i))) go(i - 1)
      else false
    }

    if (as.length <= 1) true
    else go(as.length - 2)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => partial1(a, f)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(factorial(30))
    println(Range(0, 20).map(fib))
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))

    def f(name: String, n: Int, g: Int => Int): Unit =
      println(formatResult(name, n, g))

    f("absolute value", -42, abs)
    f("factorial", 7, factorial)
    f("increment", 7, (x: Int) => x + 1)
    f("increment2", 7, (x) => x + 1)
    f("increment3", 7, x => x + 1)
    f("increment4", 7, _ + 1)
    f("increment5", 7, x => { val r = x + 1; r })

    val lessThan = new Function2[Int, Int, Boolean] {
      def apply(a: Int, b: Int) = a < b
    }
    println("10 is less than 20: %s".format(lessThan(10, 20)))
    println("10 is less than 20: %s".format(lessThan.apply(10, 20)))

    // binarySearch
    val ints = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).map(_ + 10)
    println(binarySearch(ints, 17, (a: Int, b: Int) => a > b))

    val doubles = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).map(_ + 10.0)
    println(binarySearch(doubles, 17.0, (a: Double, b: Double) => a > b))

    // isSorted
    println("is sorted: %s".format(isSorted(ints, (a: Int, b: Int) => a > b)))
    println("is sorted: %s".format(isSorted(doubles, (a: Double, b: Double) => a > b)))
    println("is sorted: %s".format(isSorted(Array(1, 5, 4, 0, 2, 3), (a: Int, b: Int) => a > b)))

    // compose
    def pow2(a: Double): Double = math.pow(a, 2)
    def div2(a: Double): Double = a / 2
    println("compose: %f".format(compose(div2, pow2)(5)))
  }
}
