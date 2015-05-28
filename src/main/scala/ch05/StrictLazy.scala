package fpinscala.strictandlazy

object StrictLazy {
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  def pair(i: => Int) = (i, i)
  def pair2(i: => Int) = { lazy val j = i; (j, j) }
}
