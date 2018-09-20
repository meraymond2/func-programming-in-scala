package functional.Chapter7

// The first implementation of Par as an abstract interface
trait Par1[_] {

  // unit in Scala, return in Haskell, sometimes bind
  def unit[A](a: A): Par1[A]

  def fork[A](a: => Par1[A]): Par1[A]

  def async[A](a: => A): Par1[A] = fork(unit(a))

  def run[A](a: Par1[A]): A

  def map2[A,B,C](a: => Par1[A], b: => Par1[B])(f: (A,B) => C): Par1[C]

}

