package functional.chapter4

// again, changing names to avoid collisions
sealed trait Result[+E, +A] {

  // Exercise 7 — map, flatMap, orElse, map2
  def map[B](f: A => B): Result[E, B] = this match {
    case Good(a) => Good(f(a))
    case Bad(e)  => Bad(e)
  }

  // I wouldn’t have to use pattern matching everywhere if I wrote some extra getters,
  // though I don’t know that it’s slower than if/else statements.
  def flatMap[EE >: E, B](f: A => Result[EE, B]): Result[EE,B] = this match {
    case Bad(e) => Bad(e)
    case Good(a) => f(a)
  }

  def orElse[EE >: E,B >: A](b: => Result[EE, B]): Result[EE, B] = this match {
    case Good(a) => Good(a)
    case Bad(_)  => b
  }

  def map2[EE >: E, B, C](b: Result[EE,B])(f: (A, B) => C): Result[EE, C] = this match {
    case Bad(e) => Bad(e)
    case Good(a) => b match {
      case Bad(ee) => Bad(ee)
      case Good(bb) => Good(f(a, bb))
    }
  }

  // Exercise 8 — sequence and traverse
  /*
    Another day
   */

}
case class Bad[+E](value: E) extends Result[E, Nothing]
case class Good[+A](value: A) extends Result[Nothing, A]

object Chapter4Either {
  def mean(xs: IndexedSeq[Double]): Result[String, Double] =
    if (xs.isEmpty)
      Bad("mean of empty list!")
    else
      Good(xs.sum / xs.length)

  def safeDiv(x: Double, y: Double): Result[String, Double] =
    if (y != 0)
      Good(x / y)
    else
      Bad("Divide by 0 error.")
}