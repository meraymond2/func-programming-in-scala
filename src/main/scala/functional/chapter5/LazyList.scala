package functional.chapter5

/* The chapter is describing Streams. */
trait LazyList[+A] {
  def uncons: Option[(A, LazyList[A])]

  def isEmpty: Boolean = uncons.isEmpty

  /* Exercise 1 */
  def toList: List[A] = this.uncons match {
    case Some((x, xs)) => x :: xs.toList
    case None          => Nil
  }

  /* Exercise 2 */
  def take(n: Int): LazyList[A] = {
    @annotation.tailrec
    def loop(from: LazyList[A], to: List[A], i: Int): LazyList[A] = from.uncons match {
      case _ if i == n =>
        LazyList(to:_*)
      case None =>
        LazyList(to:_*)
      case Some((head, tail)) =>
        loop(tail, to :+ head, i + 1)
    }
    loop(this, List.empty[A], 0)
  }

  /*
  These confused me a bit, because there is no way to append to the stream. For these, I think
  it's ok to use a normal List for the return, because they should always be finite, even if the
  source is infinite.

  Is it possible to append without iterating through the stream?
  */

  /* Exercise 3 */
  def takeWhile(p: A => Boolean): LazyList[A] = {
    @annotation.tailrec
    def loop(from: LazyList[A], to: List[A]): LazyList[A] = from.uncons match {
      case Some((head, tail)) if p(head) =>
        loop(tail, to :+ head)
      case _ =>
        LazyList(to:_*)
    }
    loop(this, List.empty[A])
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  /*
  So f's second parameter, b in the following, is only evaluated
  if you use it. And if it's not evaluated, it doesn't continue the loop,
  because the next iteration is only happening in b's evaluation.
  :headache:
   */

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /* Exercise 4 */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /* Exercise 5 */
  def takeWhile2(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, b) =>
      if (p(a)) LazyList.cons(a, b) else LazyList.empty[A]
    )

  /* Exercise 6a */
  def map[B](f: A => B): LazyList[B] =
    foldRight(LazyList.empty[B])((a, b) =>
      LazyList.cons(f(a), b)
    )

  /* Exercise 6b */
  def filter(p: A => Boolean): LazyList[A] =
    foldRight(LazyList.empty[A])((a, b) =>
      if (p(a)) LazyList.cons(a, b) else b
    )

  /* Exercise 6c */
  def append[B >: A](x: B): LazyList[B] =
    foldRight(LazyList(x))((a, b) => LazyList.cons(a, b))

  /* Exercise 6d */
  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    map(f).foldRight(LazyList.empty[B])((a, b) =>
      a.foldRight(b)((aa, bb) =>
        LazyList.cons(aa, bb)
      )
    )

}

object LazyList {

  def empty[A]: LazyList[A] =
    new LazyList[A] { def uncons = None }

  def cons[A](head: => A, tail: => LazyList[A]): LazyList[A] =
    new LazyList[A] {
      lazy val uncons = Some(head, tail)
    }

  def apply[A](as: A*): LazyList[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  /* Exercise 7 */
  def constant[A](a: A): LazyList[A] =
    cons(a, constant(a))

  /* Exercise 8 */
  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  /* Exercise 9 */
  def fibs: LazyList[Int] = {
    def loop(a: Int, b: Int): LazyList[Int] =
      cons(a + b, loop(b, a + b))
    cons(0, cons(1, loop(0, 1)))
  }

  /* Exercise 10 */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(z) match {
      case Some((a, b)) =>
        cons(a, unfold(b)(f))
      case None => LazyList.empty[A]
    }

  /* Exercise 11 */
  def fibs2: LazyList[Int] =
    cons(0, cons(1, unfold((0, 1))(i => {
      val (a, b) = i
      Some(a + b, (b, a + b))
    })))

  def from2(from: Int): LazyList[Int] =
    unfold(from)(i => Some(i, i + 1))

  def constant2[A](a: A): LazyList[A] =
    unfold(a)(_ => Some(a, a))

  def twos: LazyList[Int] =
    unfold(2)(_ => Some(2, 2))

  /* Exercise 12 */
  // TODO

}