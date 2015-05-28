package fpinscala.errorhandling

object Option {
  sealed trait Option[+A]
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  //def map[B](f: A => B): Option[B] = this match {
  //  case None => None
  //  case Some(x) => Some(f(x))
  //}

  //def flatMap[B](f: A => Option[B]): Option[B]
  //def getOrElse[B >: A](default: => B): B
  //def orElse[B >: A](obj: => Option[B]): Option[B]
  //def filter(f: A => Boolean): Option[A]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}

object ErrorHandling {
  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  case class Employee(name: String, department: String)

  val employeesByName: Map[String, Employee] =
    List(Employee("Alice", "R&D"), Employee("Bob", "Accounting"))
      .map(e => (e.name, e)).toMap

  val dept: Option[String] = employeesByName.get("Joe").map(x => x.department)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  import java.util.regex._
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)

  def doesMatch(pat: String, s: String): Option[Boolean] =
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat) flatMap (f =>
    mkMatcher(pat2) map (g =>
      f(s) && g(s)))

  sealed trait Either[+E, +A]
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def mean_either(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty List!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Double, y: Double): Either[Exception, Double] =
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }
}
