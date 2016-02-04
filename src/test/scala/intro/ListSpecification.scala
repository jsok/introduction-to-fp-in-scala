package intro

import org.scalacheck._, Arbitrary._, Gen._, Prop._

object ListSpecification extends Properties("List") {
  /*
   * Example: Verify that that our Lists.length matches the
   * builtin List#size
   */
  property("Lists#length matches standard library") =
    forAll((xs: List[Int]) => Lists.length(xs) == xs.size)

  /* Exercise 1 */
  property("Lists#append - the length of the result is equal to the sum of the two input lengths") =
    forAll((xs: List[Int], ys: List[Int]) => Lists.append(xs, ys).length == (xs.size + ys.size))

  /* Exercise 2 */
  property("Lists#append - every element in the first and second list appears in the result") =
    forAll((xs: List[Int], ys: List[Int]) => Lists.append(xs, ys).containsSlice(xs)) &&
    forAll((xs: List[Int], ys: List[Int]) => Lists.append(xs, ys).containsSlice(ys))


  /* Exercise 3 */
  property("Lists#filter - filter(_ => false) always gives empty list") =
    forAll((xs: List[Int]) => Lists.filter(xs)(_ => false) == Nil)

  /* Exercise 4 */
  property("Lists#filter - filter(_ => true) always gives input list") =
    forAll((xs: List[Int]) => Lists.filter(xs)(_ => true) == xs)

  /* Exercise 5 */
  property("Lists#filter - length of output is always less than or equal to length of input") =
    forAll((xs: List[Int], p: (Int => Boolean)) => Lists.filter(xs)(p).length <= xs.length)

  /* *Challenge* exercise 6
     Identify a set of properties that together with the type signature
     guarantees the validity of your reverse function (assuming pure-total FP) */
  property("Lists#reverse - length of output is always the same as input") =
    forAll((xs: List[Int]) => Lists.reverse((xs)).length == xs.length)

  property("Lists#reverse - double reverse results in original list") =
    forAll((xs: List[Int]) => Lists.reverse(Lists.reverse(xs)) == xs)

  /* *Challenge* exercise 7
     Identify a set of properties for testing sequence */
  property("Lists#sequence - Output length is either same as input or 0") =
    forAll((xs: List[Option[Int]]) => Lists.sequence(xs) match {
      case Some(v) => v.length == xs.length
      case None => true
    })

  property("Lists#sequence - If all inputs are Some, they should be present in the output") =
    forAll((xs: List[Int]) => Lists.sequence(xs.map(x => Some(x))) match {
      case None => false
      case Some(v) => v == xs
    })

}
