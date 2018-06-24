package functional.chapter4

import java.util.regex._

// Changing the names to avoid collisions with standard lib
sealed trait Maybe[+A] {

  // exercise 1
  def map[B](f: A => B): Maybe[B] = this match {
    case Just(a) => Just(f(a))
    case Nix     => Nix
  }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = this match {
    case Nix     => Nix
    case Just(a) => f(a) match {
      case Nix     => Nix
      case Just(b) => Just(b)
    }
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Nix     => default
    case Just(a) => a
  }

  def orElse[B >: A](ob: => Maybe[B]): Maybe[B] = this match {
    case Nix => ob
    case _ => this
  }

  def filter(f: A => Boolean): Maybe[A] = this match {
    case Just(a) if f(a) => this
    case _ => Nix
  }

  // What does it mean ‘resort to pattern matching’, why not pattern match?
  def filter2(f: A => Boolean): Maybe[A] =
    if (this != Nix) {
      val Just(get) = this
      if (f(get)) this else Nix
    }
    else Nix
}

case class Just[+A](get: A) extends Maybe[A]
case object Nix extends Maybe[Nothing]

object Chapter4 {

  def mean(xs: Seq[Double]): Maybe[Double] = if (xs.isEmpty) Nix else Just(xs.sum)

  def variance(xs: Seq[Double]): Maybe[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  /*
    this works because map and flatMap are defined on Maybe,
    if you comment them out you get:
      value flatMap is not a member of functional.chapter4.Maybe[A]
      value map is not a member of functional.chapter4.Maybe[B]
    the for comprehension is just sugar for these functions, and can
    be used anywhere they're defined. If you add in an `if` you need
    to define `withFilter`
   */
  def map2[A,B,C](a: Maybe[A], b: Maybe[B])(f: (A, B) => C): Maybe[C] =
    for {
      x <- a
      y <- b
    } yield f(x, y)

  def pattern(s: String): Maybe[Pattern] =
    try {
      Just(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => Nix
    }

  def bothMatch_2(pat1: String, pat2: String, s: String): Maybe[Boolean] =
    map2(pattern(pat1), pattern(pat2))((p1, p2) =>
      p1.matcher(s).matches && p2.matcher(s).matches
    )

  // Exercise 5
  def sequence[A](a: List[Maybe[A]]): Maybe[List[A]] = a.foldRight[Maybe[List[A]]](Just(List.empty[A]))((opt, z) => z match {
    case Nix        => Nix
    case Just(list) => opt match {
      case Nix         => Nix
      case Just(value) => Just(value :: list)
    }
  })

  // Exercise 6
  def traverse[A, B](a: List[A])(f: A => Maybe[B]): Maybe[List[B]] = a.foldRight[Maybe[List[B]]](Just(List.empty[B]))((i, z) => z match {
    case Nix => Nix
    case Just(list) => f(i) match {
      case Nix         => Nix
      case Just(value) => Just(value :: list)
    }
  })
}
