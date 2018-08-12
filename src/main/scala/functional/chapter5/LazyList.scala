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
  Update: probably not, but there are prettier ways to do it with foldRight and unfold.
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

  /* Exercise 12 */
  def map2[B](f: A => B): LazyList[B] =
    LazyList.unfold(uncons) {
      case None         => None
      case Some((h, t)) => Some((f(h), t.uncons))
    }

  def take2(n: Int): LazyList[A] =
    LazyList.unfold((uncons, 0)) {
      case (Some((h, t)), i) if i < n => Some(h, (t.uncons, i + 1))
      case _                          => None
    }

  def takeWhile3(p: A => Boolean): LazyList[A] =
    LazyList.unfold(uncons) {
      case Some((h, t)) if p(h) => Some(h, t.uncons)
      case _                    => None
    }

  def zip[B](ll: LazyList[B]): LazyList[(A, B)] =
    LazyList.unfold((uncons, ll.uncons)) {
      case (Some((h1, t1)), Some((h2, t2))) => Some((h1, h2), (t1.uncons, t2.uncons))
      case _                                => None
    }

  def zipAll[B](ll: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold((uncons, ll.uncons)) {
      case (Some((h1, t1)), Some((h2, t2))) => Some((Some(h1), Some(h2)), (t1.uncons, t2.uncons))
      case (Some((h1, t1)), None)           => Some((Some(h1), None), (t1.uncons, None))
      case (None, Some((h2, t2)))           => Some((None, Some(h2)), (None, t2.uncons))
      case (None, None)                     => None
    }

  /* Exercise 13 */
  def startsWith[B >: A](ss: LazyList[B]): Boolean = {
    val zipped = zip(ss)
    if (zipped.isEmpty) false
    else zipped.foldRight(true) { case ((h1, h2), z) => h1 == h2 && z }
  }

  /* Exercise 14 */
  def tails: LazyList[LazyList[A]] =
    LazyList.cons(
      head = this,
      LazyList.unfold(uncons) {
        case Some((_, t)) => Some(t, t.uncons)
        case None         => None
      }
    )

  def hasSubsequence[B >: A](ss: LazyList[B]): Boolean =
    tails.exists(_.startsWith(ss))

  /* Exercise 15 */
  def scanRight[B](z: B)(f: (A, => B) => B): LazyList[B] =
    LazyList.unfold(uncons) {
      case Some((h, t)) => Some(f(h, t.foldRight(z)(f)), t.uncons)
      case None         => None
    }.append(z)

  def scanRight2[B](z: B)(f: (A, => B) => B): LazyList[B] =
    tails.map(_.foldRight(z)(f))
  // these both work but I don't think either it O(n)

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
      case Some((a, b)) => cons(a, unfold(b)(f))
      case None         => LazyList.empty[A]
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

}