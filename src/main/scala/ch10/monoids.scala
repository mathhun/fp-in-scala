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

  def wordsMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String =
      a1.trim + " " + a2.trim
    def zero = ""
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), m)

  //def foldRight2[A, B](zero: B)(f: A => A => B): B =
  //  zero

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def wcMonoid = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC =
      zero
    def zero = Part("", 0, "")
  }

  def countWords(s: String): Int =
    0

  // F[_] == higher-order type constructor
  def Foldale[F[_]] {
    //def foldRight[A, B](as: F[A])(f: (A, B) => B): B
    //def foldLeft[A, B](as: F[A])(f: (B, A) => B): B
    //def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    //def concatenate[A](as: F[A])(m: Monoid[A]): A
  }
}
