package functional.chapter4

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
}
case class Just[+A](get: A) extends Maybe[A]
case object Nix extends Maybe[Nothing]

