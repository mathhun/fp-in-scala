package fpinscala.monad

object Monad {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))

    trait listFunctor extends Functor[List] {
      def map[A, B](as: List[A])(f: A => B): List[B] = as map f
    }
  }

  trait Monad[M[_]] extends Functor[M] {
    def unit[A](a: => A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => unit(f(a)))
    def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
      flatMap(ma)(a => map(mb)(b => f(a, b)))

    def sequence[A](lma: List[M[A]]): M[List[A]]
    def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]]

    def replicateM[A](n: Int, ma: M[A]): M[List[A]]

    def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
      (a: A) => flatMap(f(a))(g)
  }

  case class Order(item: Item, quantity: Int)
  case class Item(name: String, price: Double)

  //val genOrder: Gen[Order] = for {
  //  name <- Gen.nextString
  //  price <- Gen.nextDouble
  //  quantity <- Gen.nextInt
  //} yield Order(Item(name, price), quantity)
}
