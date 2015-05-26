package fpinscala.errorhandling

object ErrorHandling {
  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length
}

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

object Emp {
  case class Employee(name: String, department: String)

  val employeesByName: Map[String, Employee] =
    List(Employee("Alice", "R&D"), Employee("Bob", "Accounting"))
      .map(e => (e.name, e)).toMap

  val dept: Option[String] = employeesByName.get("Joe").map(x => x.department)
}
