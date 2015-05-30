package fpinscala.monoids

object Monoids {
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def zero = Nil
  }

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero = 1
  }

  def booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero = false
  }

  def booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 match {
      case Some(x) => Some(x)
      case None => a2
    }
    def zero = None
  }

  def EndoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A =
      (x: A) => a1(a2(x))
    def zero = (x: A) => x
  }
}
