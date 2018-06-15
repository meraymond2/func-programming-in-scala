package functional.chapter2

object Exercises {

  def fib(num: Int): Int = {

    @annotation.tailrec
    def loop(n: Int, acc1: Int, acc2: Int): Int =
      if (n == num - 1) acc1 + acc2
      else loop(n + 1, acc1 + acc2, acc1)


    loop(1, 1, 0)
  }

  def isSorted[T](as: Array[T], gt: (T, T) => Boolean): Boolean = {

    @annotation.tailrec
    def loop(i: Int, acc: Boolean): Boolean =
      if (i == as.length - 1) acc
      else loop(i + 1, acc && gt(as(i + 1), as(i)))

    loop(0, acc = true)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    f(a, _)

  def curry[A,B,C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  val l: List[Int] = List(2, 3, 4)


}