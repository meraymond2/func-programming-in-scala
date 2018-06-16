package functional.chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // exercise 25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // exercise 26
  def maximum(t: Tree[Int]): Int = {
    def loop(tt: Tree[Int], z: Int): Int = tt match {
      case Leaf(i)      => i max z
      case Branch(l, r) => loop(l, z) max loop(r, z)
    }
    loop(t, 0)
  }

  // exercise 27
  def depth[A](t: Tree[A]): Int = {
    def loop(tt: Tree[A], z: Int): Int = tt match {
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + (loop(l, z) max loop(r, z))
    }
    loop(t, 0)
  }

  // exercise 28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a)      => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // exercise 29 — can this be done with just one function?
  def fold[A, B](t: Tree[A], z: B)(f: (A,B) => B)(f2: (B,B) => B): B = t match {
    case Leaf(a)      => f(a, z)
    case Branch(l, r) => f2(fold(l, z)(f)(f2), fold(r, z)(f)(f2))
  }

}