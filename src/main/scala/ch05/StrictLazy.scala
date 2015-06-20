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

    def takeUnfold(n: Int): Stream[A] = {
      Stream.unfold((n, this))(s => s._2.uncons match {
        case None => None
        case Some((h, t)) =>
          if (s._1 <= 0) None
          else Some(h, (s._1 - 1, t))
      })
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

    def takeWhileUnfold(p: A => Boolean): Stream[A] = {
      Stream.unfold(this)(s => s.uncons match {
        case None => None
        case Some((h, t)) => if (p(h)) Some(h, t) else None
      })
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      uncons match {
        case Some((h, t)) => f(h, t.foldRight(z)(f))
        case None => z
      }

    def foldRight2(z: => Stream[Int])(f: (Int, => Stream[Int]) => Stream[Int]): Stream[Int] =
      uncons match {
        case Some((h: Int, t)) => f(h, t.foldRight2(z)(f))
        case Some(x) => f(0, x._2.foldRight2(z)(f))
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

    def map[B](f: A => B): Stream[B] = uncons match {
      case None => Stream.empty
      case Some((h, t)) => Stream.cons(f(h), t.map(f))
    }

    def mapUnfold[B](f: A => B): Stream[B] =
      Stream.unfold(this)(s => s.uncons match {
        case Some((h, t)) => Some((f(h), t))
        case None => None
      })

    def zipUnfold[B](bs: Stream[B]): Stream[(A, B)] = {
      def g(as: Stream[A], bs: Stream[B]): Option[((A, B), (Stream[A], Stream[B]))] = {
        (as.uncons, bs.uncons) match {
          case (None, _) => None
          case (_, None) => None
          case (Some((h1, t1)), Some((h2, t2))) => Some(((h1, h2), (t1, t2)))
        }
      }
      Stream.unfold((this, bs))(s => g(s._1, s._2))
    }

    def zipWithUnfold[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = {
      def g(as: Stream[A], bs: Stream[B]): Option[(C, (Stream[A], Stream[B]))] = {
        (as.uncons, bs.uncons) match {
          case (None, _) => None
          case (_, None) => None
          case (Some((h1, t1)), Some((h2, t2))) => Some((f(h1, h2), (t1, t2)))
        }
      }
      Stream.unfold((this, bs))(s => g(s._1, s._2))
    }

    def tails(): Stream[Stream[A]] = {
      Stream.unfold((this, false))(s => s._1.uncons match {
        case Some((h, t)) => Some((s._1, (t, false)))
        case None => if (s._2) None else Some((s._1, (Stream.empty, true)))
      })
    }

    override def toString(): String =
      "Stream(" + toList.mkString(",") + ")"
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

    def fibsUnfold(): Stream[Int] = {
      unfold(0, 1)(s => Some(s._1, (s._2, s._1 + s._2)))
    }

    def fromUnfold(n: Int): Stream[Int] =
      unfold(n)(m => Some(m, m + 1))

    def constantUnfold(n: Int): Stream[Int] =
      unfold(n)(m => Some(m, m))

    def startsWith[A](s1: Stream[A], s2: Stream[A]): Boolean =
      s1.zipUnfold(s2).forAll(s => s._1 == s._2)
  }

  val ones: Stream[Int] = Stream.cons(1, ones)

  def fibs(): Stream[Int] = {
    def f(n0: Int, n1: Int): Stream[Int] = Stream.cons(n0, f(n1, n0 + n1))
    f(0, 1)
  }

  val onesUnfold: Stream[Int] = Stream.unfold(1)(_ => Some(1, 1))
}
