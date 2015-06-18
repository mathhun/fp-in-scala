package fpinscala.strictandlazy

object StrictLazy {
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  def pair(i: => Int) = (i, i)
  def pair2(i: => Int) = { lazy val j = i; (j, j) }

  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty

    def toList: List[A] = {
      def go(as: Stream[A], acc: List[A]): List[A] = as.uncons match {
        case None => acc.reverse
        case Some((x, rest)) => go(rest, x :: acc)
      }
      go(this, Nil)
    }

    def take(n: Int): Stream[A] = {
      def go(m: Int, stream: Stream[A]): Stream[A] = stream.uncons match {
        case None => Stream.empty
        case Some((x, rest)) =>
          if (m <= 0) Stream.empty
          else Stream.cons(x, go(m - 1, rest))
      }
      go(n, this)
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      def go(as: Stream[A]): Stream[A] = as.uncons match {
        case None => Stream.empty
        case Some((x, rest)) =>
          if (p(x)) Stream.cons(x, go(rest))
          else Stream.empty
      }
      go(this)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      uncons match {
        case Some((h, t)) => f(h, t.foldRight(z)(f))
        case None => z
      }

    def foldRight2(z: => Stream[Int])(f: (Int, => Stream[Int]) => Stream[Int]): Stream[Int] =
      uncons match {
        case Some((h: Int, t)) => f(h, t.foldRight2(z)(f))
        case None => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean = uncons match {
      case None => true
      case Some((h, t)) => if (p(h)) t.forAll(p) else false
    }

    def takeWhile2(p: Int => Boolean): Stream[Int] = {
      this.foldRight2(Stream.empty)((h, t) =>
        if (p(h)) Stream.cons(h, t.takeWhile2(p))
        else Stream.empty
      )
    }

    override def toString(): String =
      toList.toString
  }

  object Stream {
    def empty[A]: Stream[A] =
      new Stream[A] { def uncons = None }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((hd, tl))
      }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] =
      cons(a, constant(a))

    def from(n: Int): Stream[Int] =
      cons(n, from(n + 1))

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  val ones: Stream[Int] = Stream.cons(1, ones)

  def fibs(): Stream[Int] = {
    def f(n0: Int, n1: Int): Stream[Int] = Stream.cons(n0, f(n1, n0 + n1))
    f(0, 1)
  }

  def fibsUnfold(): Stream[Int] = {
    Stream.unfold(0, 1)(s => Some(s._1, (s._2, s._1 + s._2)))
  }

  def fromUnfold(n: Int): Stream[Int] =
    Stream.unfold(n)(m => Some(m, m + 1))

  def constantUnfold(n: Int): Stream[Int] =
    Stream.unfold(n)(m => Some(m, m))

  val onesUnfold: Stream[Int] = Stream.unfold(1)(_ => Some(1, 1))
}
