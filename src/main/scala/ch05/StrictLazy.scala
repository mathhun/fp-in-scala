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

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean = uncons match {
      case None => true
      case Some((h, t)) => if (p(h)) t.forAll(p) else false
    }

    def takeWhile2(p: A => Boolean): Stream[A] = {
      this.foldRight(Stream.empty)((h, t) =>
        if (p(h)) Stream.cons(h, Stream.empty)
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
  }
}
