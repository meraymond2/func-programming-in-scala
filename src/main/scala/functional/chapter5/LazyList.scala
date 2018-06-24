package functional.chapter5

// The chapter is describing Streams.
trait LazyList[+A] {
  def uncons: Option[(A, LazyList[A])]

  def isEmpty: Boolean = uncons.isEmpty

  // Exercise 1
  def toList: List[A] = this.uncons match {
    case Some((x, xs)) => x :: xs.toList
    case None          => Nil
  }

  // Exercise 2
  def take(n: Int): LazyList[A] = this // do later
}

object LazyList {

  def empty[A]: LazyList[A] =
    new LazyList[A] { def uncons = None }

  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    new LazyList[A] {
      lazy val uncons = Some(hd, tl)
    }

  def apply[A](as: A*): LazyList[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}