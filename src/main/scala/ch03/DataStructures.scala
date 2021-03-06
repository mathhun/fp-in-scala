package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    def go(m: Int, xs: List[A]): List[A] = xs match {
      case Nil => Nil
      case Cons(x, xs) => {
        if (m == 1) xs
        else go(m - 1, xs)
      }
    }

    if (n < 0) Nil
    else if (n == 0) l
    else go(n, l)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs)(f)
      else l
  }

  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(a, xs)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_: A, len: Int) => len + 1)

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    def go(l: List[A], acc: B): B = l match {
      case Nil => acc
      case Cons(x, xs) => go(xs, f(acc, x))
    }
    go(l, z)
  }

  def sum3(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((len: Int, _: A) => len + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((a, b) => Cons(b, a))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def flatten[A](ls: List[List[A]]): List[A] = {
    //def cons(xss: List[A], yss: List[A]) = {
    //  tail = ys match {
    //    case Nil => Nil
    //    case Cons(y, ys) => cons(ys, Nil)
    //  }
    //}
    foldRight(ls, Nil: List[A])((a1: List[A], a2: List[A]) => append(a1, a2))
  }

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)
}
