package functional.chapter2

import org.scalatest._

class ExercisesSpec extends FlatSpec with Matchers {
  "fib" should "return the nth Fibonacci number" in {
    Exercises.fib(2) shouldEqual 1
    Exercises.fib(3) shouldEqual 2
    Exercises.fib(4) shouldEqual 3
    Exercises.fib(5) shouldEqual 5
    Exercises.fib(6) shouldEqual 8
    Exercises.fib(7) shouldEqual 13
  }

  "isSorted" should "return true for a sorted array" in {
    val numArray = Array(1, 2, 3, 4, 5)
    Exercises.isSorted[Int](numArray, _ > _) shouldEqual true

    val unsortedArray = Array(6, 2, 8, 4)
    Exercises.isSorted[Int](unsortedArray, _ > _) shouldEqual false

    val stringArray = Array("a", "b", "c")
    Exercises.isSorted[String](stringArray, _ > _) shouldEqual true

    val unsortedStringArray = Array("z", "d", "jjj")
    Exercises.isSorted[String](unsortedStringArray, _ > _) shouldEqual false
  }

  "partial1" should "return a partially applied function" in {
    val f2 = Exercises.partial1[Int, Int, Int](10, _ * _)
    f2(5) shouldEqual 50
    f2(3) shouldEqual 30
  }

  "curry" should "return a curried function" in {
    def f(a: Int, b: Int): Int = a + b

    val f2 = Exercises.curry(f)

    f2(5)(8) shouldEqual 13
  }

  "uncurry" should "return uncurry a function" in {
    def f(a: Int): Int => Int = (b: Int) => a + b

    val f2 = Exercises.uncurry(f)

    f2(5, 8) shouldEqual 13
  }

  "compose" should "return the result of two functions" in {
    val f = (a: Int) => a * 10
    val f2 = (a: Int) => a - 1

    val f3 = Exercises.compose(f, f2)
    f3(5) shouldEqual 40

    val f4 = f compose f2
    f4(5) shouldEqual 40

    val f5 = f2 andThen f
    f5(5) shouldEqual 40
  }
}
