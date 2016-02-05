package intro

/**
 * Abstraction of types that can be mapped over.
 *
 * The following laws should hold:
 *
 *  1) map(r)(z => z) == r
 *  2) map(r)(z => f(g(z))) == map(map(r)(g))(f)
 */
trait Functor[F[_]] {
  def map[A, B](a: F[A])(f: A => B): F[B]
}

object Functor {
  /**
   * Convenience for summoning a Functor instance.
   *
   * usage: Functor[Int].map(1, 2)
   */
  def apply[F[_]: Functor]: Functor[F] =
    implicitly[Functor[F]]

  /* Functor Instances (cheating is good) */

  implicit def IdFunctor: Functor[Id] = new Functor[Id] {
    def map[A, B](a: Id[A])(f: A => B) =
      a.map(f)
  }

  implicit def OptionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](a: Option[A])(f: A => B) =
      a.map(f)
  }

  implicit def OptionalFunctor: Functor[Optional] = new Functor[Optional] {
    def map[A, B](a: Optional[A])(f: A => B) =
      a.map(f)
  }

  implicit def ListFunctor: Functor[List] = new Functor[List] {
    def map[A, B](a: List[A])(f: A => B) =
      Lists.map(a)(f)
  }

  /* Functor Library */

  /** Exercise: 1 - Twin all `A`s in `fa`.
    *
    * Example:
    * scala> Functor.fpair(Id(1))
    * res: intro.Id[(Int, Int)] = Id((1,1))
    */
  def fpair[F[_]: Functor, A](fa: F[A]): F[(A, A)] =
    Functor[F].map(fa)(a => (a, a))

  /** Exercise: 2 - Pair all `A`s in `fa` with the result of function application.
    *
    * Example:
    * scala> Functor.fproduct(List(1, 2, 3))(x => x + 1)
    * res: List[(Int, Int)] = List((1,2), (2,3), (3,4))
    */
  def fproduct[F[_]: Functor, A, B](fa: F[A])(f: A => B): F[(A, B)] =
    Functor[F].map(fa)(a => (a, f(a)))

  /** Exercise: 3 - Inject `a` to the left of `B`s in `f`.
    *
    * Example:
    * scala> Functor.strengthL("hello", List(1, 2, 3))
    * res: List[(String, Int)] = List((hello,1), (hello,2), (hello,3))
    */
  def strengthL[F[_]: Functor, A, B](a: A, f: F[B]): F[(A, B)] =
    Functor[F].map(f)(b => (a, b))

  /** Exercise: 4 - Inject `b` to the right of `A`s in `f`.
    *
    * Example:
    * scala> Functor.strengthR(List(1, 2, 3), "hello")
    * res: List[(Int, String)] = List((1,hello), (2,hello), (3,hello))
    */
  def strengthR[F[_]: Functor, A, B](f: F[A], b: B): F[(A, B)] =
    Functor[F].map(f)(a => (a, b))
}
