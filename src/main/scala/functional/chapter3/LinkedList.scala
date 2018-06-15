package functional.chapter3

/*
  sealed trait — all implementations must be declared in same file
  it's different from an abstract class, because it has no constructor
 */
sealed trait LinkedList[+A]
case object Nil extends LinkedList[Nothing]
case class Cons[+A](head: A, tail: LinkedList[A]) extends LinkedList[A]

object LinkedList {
  def sum(ints: LinkedList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: LinkedList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): LinkedList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: LinkedList[A]): LinkedList[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def drop[A](n: Int, l: LinkedList[A]): LinkedList[A] = {
    @annotation.tailrec
    def loop(remaining: LinkedList[A], counter: Int = 0): LinkedList[A] = remaining match {
      case _ if counter >= n => remaining
      case Cons(_, xs)       => loop(xs, counter + 1)
      case Nil               => Nil
    }

    loop(l)
  }

  def dropWhile[A](l: LinkedList[A])(f: A => Boolean): LinkedList[A] = {
    @annotation.tailrec
    def loop(remaining: LinkedList[A]): LinkedList[A] = remaining match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => loop(xs)
      case _ => remaining
    }

    loop(l)
  }

  def setHead[A](l: LinkedList[A], head: A): LinkedList[A] = l match {
    case Nil => Cons(head, Nil)
    case _ => Cons(head, l)
  }

  def append[A](a1: LinkedList[A], a2: LinkedList[A]): LinkedList[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def init[A](l: LinkedList[A]): LinkedList[A] = {
    @annotation.tailrec
    def loop(remaining: LinkedList[A], newList: LinkedList[A] = Nil): LinkedList[A] = remaining match {
      case Nil => Nil
      case Cons(_, Nil) => newList
      case Cons(x, xs) => loop(xs, append(newList, Cons(x, Nil)))
    }

    loop(l)
  }

  // fold—from—right
  def foldRight[A,B](l: LinkedList[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](l: LinkedList[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  // fold—from—left
  def foldLeft[A,B](l: LinkedList[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(list: LinkedList[A], acc: B = z): B = list match {
      case Nil => acc
      case Cons(x, xs) => loop(xs, f(acc, x))
    }

    loop(l)
  }

  def foldLeft2[A,B](l: LinkedList[A], z: B)(f: (B, A) => B): B =
    foldRight(l, z)((a: A, b: B) => f(b, a))

  def reverse[A](l: LinkedList[A]): LinkedList[A] =
    foldLeft(l, LinkedList[A]())((z, a) => Cons(a, z))

  def foldAppend[A](l: LinkedList[A], a: A): LinkedList[A] =
    foldRight(l, LinkedList(a))((b: A, z: LinkedList[A]) => Cons(b, z))

  def flatten[A](l: LinkedList[LinkedList[A]]): LinkedList[A] =
    foldRight(l, LinkedList[A]())((ll: LinkedList[A], z: LinkedList[A]) =>
      foldRight(ll, z)((li: A, zz: LinkedList[A]) => Cons(li, zz))
    )

  def addOne(l: LinkedList[Int]): LinkedList[Int] =
    foldRight(l, LinkedList[Int]())((li: Int, ll: LinkedList[Int]) => Cons(li + 1, ll))

  def addOne2(l: LinkedList[Int]): LinkedList[Int] =
    reverse(foldLeft(l, LinkedList[Int]())((ll: LinkedList[Int], li: Int) => Cons(li + 1, ll)))

  def map[A,B](l: LinkedList[A])(f: A => B): LinkedList[B] =
    foldRight(l, LinkedList[B]())((a: A, z: LinkedList[B]) => Cons(f(a), z))

  def filter[A](l: LinkedList[A])(f: A => Boolean): LinkedList[A] =
    foldRight(l, LinkedList[A]())((a: A, z: LinkedList[A]) =>
      if (f(a)) Cons(a, z) else z
    )
}