package intro

/*
 * Optional is a data type used to represent a value which may
 * or may not be set. An optional may either be _Full_ and contain
 * a value _or_ be _Empty_ and not contain a value.
 *
 * This is directly equivalent to the Option type in the scala
 * standard library where Some -> Full, and None -> Empty.
 */
sealed trait Optional[A] {
  /*
   * Implement fold for Optional.
   *
   * We often want to work with data structures be breaking them
   * down by cases. All algebraic data structures can be broken
   * down in the same way, a function is provided to handle each
   * case, with the arguments to the data constructors matching the
   * arguments to the fold functions.
   *
   * scala> Full(1).fold(x => x, 0)
   *  = 1
   *
   * scala> Empty[Int]().fold(x => x, 0)
   *  = 0
   */
  def fold[X](
    full: A => X,
    empty: => X
  ): X = this match {
    case Full(v) => full(v)
    case Empty() => empty
  }

  /*
   * Implement map for Optional[A].
   *
   * The following laws must hold:
   *  1) r.map(z => z) == r
   *  2) r.map(z => f(g(z))) == r.map(g).map(f)
   *
   * scala> Full(1).map(x => x + 10)
   *  = Full(11)
   *
   * scala> Empty[Int]().map(x => x + 10)
   *  = Empty()
   */
  def map[B](f: A => B): Optional[B] =
    this match {
      case Full(v) => Full(f(v))
      case Empty() => Empty[B]()
    }

  /*
   * Implement flatMap.
   *
   * The following law must hold:
   *   r.flatMap(f).flatMap(g) == r.flatMap(z => f(z).flatMap(g))
   *
   * scala> Full(1).flatMap(x => Full(x + 10))
   *  = Full(11)
   *
   * scala> Full(1).flatMap(x => Full(x + 10)).flatMap(x => Full(x - 10))
   *  = Full(1)
   *
   * scala> Full(1).flatMap(x => Empty[Int]())
   *  = Empty(Unauthorized)
   *
   * scala> Empty[Int]().flatMap(x => Full(x + 10))
   *  = Empty()
   *
   * Advanced: Try using fold.
   */
  def flatMap[B](f: A => Optional[B]): Optional[B] = fold(f, Empty[B]())

  def flatMap2[B](f: A => Optional[B]): Optional[B] =
    this match {
      case Empty() => Empty[B]()
      case Full(v) => f(v)
    }

  /*
   * Extract the value if it is success case otherwise use default value.
   *
   *
   * scala> Full(1).getOrElse(10)
   *  = 1
   *
   * scala> Empty[Int]().getOrElse(10)
   *  = 10
   */
  def getOrElse(otherwise: => A): A =
    fold(x => x, otherwise)

  /*
   * Implement choice, take this result if successful otherwise take
   * the alternative.
   *
   * scala> Full(1) ||| Full(10)
   *  = Full(1)
   *
   * scala> Full(1) ||| Empty[Int]()
   *  = Full(1)
   *
   * scala> Empty[Int]() ||| Full(10)
   *  = Full(10)
   *
   * scala> Empty[Int]() ||| Empty[Int]()
   *  = Empty()
   */
  def |||(alternative: => Optional[A]): Optional[A] =
    fold(x => Full(x), alternative)
}

case class Full[A](a: A) extends Optional[A]
case class Empty[A]() extends Optional[A]

object Optional {
  def empty[A]: Optional[A] =
    Empty()

  def ok[A](value: A): Optional[A] =
    Full(value)
}
