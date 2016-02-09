package patterns

import intro._

/*
 * A reader data type that represents the application of some
 * environment to produce a value.
 */
case class Reader[R, A](run: R => A) {

  /*
   * Exercise 2.1:
   *
   * Implement map for Reader[R, A].
   *
   * The following laws must hold:
   *  1) r.map(z => z) == r
   *  2) r.map(z => f(g(z))) == r.map(g).map(f)
   *
   * Two readers are equal if for all inputs, the same result is produced.
   */
  def map[B](f: A => B): Reader[R, B] = Reader(r => f(run(r)))

  /*
   * Exercise 2.2:
   *
   * Implement flatMap (a.k.a. bind, a.k.a. >>=).
   *
   * The following law must hold:
   *   r.flatMap(f).flatMap(g) == r.flatMap(z => f(z).flatMap(g))
   *
   * Two readers are equal if for all inputs, the same result is produced.
   */
  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(run(r)).run(r))
}

object Reader {
  /*
   * Exercise 2.3:
   *
   * Implement value  (a.k.a. return, point, pure).
   *
   * Hint: Try using Reader constructor.
   */
  def value[R, A](a: => A): Reader[R, A] = Reader(r => a)

  /*
   * Exercise 2.4:
   *
   * Implement ask.
   *
   * Ask provides access to the current environment (R).
   *
   * Hint: Try using Reader constructor.
   */
  def ask[R]: Reader[R, R] = Reader(r => r)

  /*
   * Exercise 2.5:
   *
   * Implement local.
   *
   * Local produces a reader that runs with a modified environment.
   *
   * Hint: Try using Reader constructor.
   */
  def local[R, A](f: R => R)(reader: Reader[R, A]): Reader[R, A] =
    Reader(r => reader.run(f(r)))

  /*
   * Exercise 2.6:
   *
   * Sequence, a list of Readers, to a Reader of Lists.
   */
  def sequence[R, A](readers: List[Reader[R, A]]): Reader[R, List[A]] =
    Reader(r => readers.map(_.run(r)))

  implicit def ReaderMonoid[R, A: Monoid]: Monoid[Reader[R, A]] =
    new Monoid[Reader[R, A]] {
      def identity: Reader[R, A] =
        value[R, A](Monoid[A].identity)

      def op(a: Reader[R, A], b: Reader[R, A]) =
        for { aa <- a; bb <- b } yield Monoid[A].op(aa, bb)
    }


 class Reader_[R] {
    type l[a] = Reader[R, a]
  }

  implicit def ReaderMonad[R]: Monad[Reader_[R]#l] =
    new Monad[Reader_[R]#l] {
      def point[A](a: => A): Reader[R, A] =
        value(a)

      def bind[A, B](r: Reader[R, A])(f: A => Reader[R, B]) =
        r flatMap f
    }
}


/*
 * *Challenge* Exercise 2.7: Indirection.
 *
 * Lookup a specified config value, and then use its values
 * as keys to look up a subsequent set of values.
 *
 * Complete the implementation, some of the methods are provided
 * fill in the remainder, to complete the spec.
 */
object Example {
  case class ConfigEntry(name: String, values: List[String])
  case class Config(data: List[ConfigEntry])

  /*
   * For a single name, lookup all of the direct values for that name.
   *
   * Libraries available:
   *   - The Reader.* libraries
   *   - List[A] has `find` method that will provide a Option[A]
   *   - Option[A] has a `getOrElse` method similar to challenge1.Result
   *
   * Hint: Starting with Reader.ask will help.
   */
  def direct(name: String): Reader[Config, List[String]] =
    Reader.ask.map(c => c.data.collect { case ConfigEntry(n, vs) if n == name => vs }.flatten)

  def direct2(name: String): Reader[Config, List[String]] =
    Reader(config => config.data
      .find(entry => entry.name == name)
      .getOrElse(ConfigEntry(name, List()))
      .values
    )

  /*
   * For a single name, lookup all of the indirect values, that
   * is those values whose key is one of the direct values of
   * the specified name.
   *
   * Libraries available:
   *   - List[List[A]].flatten will produce a List[A].
   *
   * Hint: Starting with Reader.sequence will be important.
   *
   * scala> Example.indirect("foo").run(Config(List(
   *   ConfigEntry("foo", List("bar", "baz")),
   *   ConfigEntry("bar", List("dog", "cat")),
   *   ConfigEntry("baz", List("qux")))
   * ))
   * res: List[String] = List(dog, cat, qux)
   */
  def indirect(name: String): Reader[Config, List[String]] = {
    def f(values: List[String]): List[Reader[Config, List[String]]] = values.map(n => direct(n))
    direct(name).flatMap(values => Reader.sequence(f(values))).map(_.flatten)
  }
}
