package functional.chapter3

import org.scalatest._

class LinkedListSpec extends FlatSpec with Matchers {
  "tail" should "return the list without the first element" in {

    val ll = LinkedList(1, 2, 3)

    LinkedList.tail(ll) shouldEqual LinkedList(2, 3)


    val ll2 = Nil
    // More Haskelly
    LinkedList tail ll2 shouldEqual Nil
  }

  "drop" should "return the list without the first n elements" in {

    val ll = LinkedList(1, 2, 3, 4, 5)

    LinkedList.drop(0, ll) shouldEqual ll
    LinkedList.drop(1, ll) shouldEqual LinkedList(2, 3, 4, 5)
    LinkedList.drop(2, ll) shouldEqual LinkedList(3, 4, 5)
    LinkedList.drop(3, ll) shouldEqual LinkedList(4, 5)
    LinkedList.drop(4, ll) shouldEqual LinkedList(5)
    LinkedList.drop(5, ll) shouldEqual LinkedList()
    LinkedList.drop(5, ll) shouldEqual Nil
    LinkedList.drop(6, ll) shouldEqual Nil
    LinkedList.drop(7, Nil) shouldEqual Nil

  }

  "dropWhile" should "drop elements until the condition is false" in {
    val ll = LinkedList(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val empty: LinkedList[Int] = Nil

    LinkedList.dropWhile(ll)(_ <= 5) shouldEqual LinkedList(6, 7, 8, 9)
    LinkedList.dropWhile(ll)(_ > 77) shouldEqual ll
    LinkedList.dropWhile(empty)(_ <= 5) shouldEqual Nil

  }

  "setHead" should "replace the first element in a linked list" in {
    val ll = LinkedList("cascat", "luna")
    val empty: LinkedList[String] = Nil

    LinkedList.setHead(ll, "sherlock") shouldEqual LinkedList("sherlock", "cascat", "luna")
    LinkedList.setHead(empty, "sherlock") shouldEqual LinkedList("sherlock")
  }

  "init" should "return all but the last element of a linked list" in {
    val ll = LinkedList("sherlock", "cascat", "luna")

    LinkedList init ll shouldEqual LinkedList("sherlock", "cascat")
    LinkedList init Nil shouldEqual Nil
  }

  "foldRight" should "reduce a list to a single value" in {
    val range = 1 to 10
    val ll = LinkedList(range:_*)

    LinkedList.foldRight(ll, 0)(_ + _) shouldEqual 55
  }

  "length" should "return the length of the linked list" in {
    val ll = LinkedList(1, 2, 3, 4, 5)

    LinkedList length ll shouldEqual 5
    LinkedList length Nil shouldEqual 0
  }

  "foldLeft" should "reduce a list to a single value" in {
    val range = 1 to 10
    val ll = LinkedList(range:_*)

    LinkedList.foldLeft(ll, 0)(_ + _) shouldEqual 55
    LinkedList.foldLeft2(ll, 0)(_ + _) shouldEqual 55

    // I'm not sure how to get this to work for all numbers. The + confuses it.
    def sum(l: LinkedList[Int]): Int = LinkedList.foldLeft(l, 0)(_ + _)

    sum(ll) shouldEqual 55

    def product(l: LinkedList[Int]): Int = LinkedList.foldLeft(l, 1)(_ * _)

    product(ll) shouldEqual 3628800

    def length[A](l: LinkedList[A]): Int = LinkedList.foldLeft(l, 0)((z, _) => z + 1)

    length(ll) shouldEqual 10
  }

  "reverse" should "return a reversed list" in {
    val ll = LinkedList("a", "b", "c", "d")

    LinkedList.reverse(ll) shouldEqual LinkedList("d", "c", "b", "a")
  }

  "append" should "add an item onto the end of a list" in {
    val ll = LinkedList("a", "b", "c", "d")

    LinkedList.foldAppend(ll, "e") shouldEqual LinkedList("a", "b", "c", "d", "e")
  }

  "flatten" should "return a list from a list of lists" in {
    val ll = LinkedList(LinkedList(1, 2), LinkedList(3, 4), LinkedList(5, 6))

    LinkedList.flatten(ll) shouldEqual LinkedList(1, 2, 3, 4, 5, 6)
  }

  "addOne" should "map the list by adding 1" in {
    val ll = LinkedList(1, 2, 3)

    LinkedList.addOne(ll) shouldEqual LinkedList(2, 3, 4)
    LinkedList.addOne2(ll) shouldEqual LinkedList(2, 3, 4)

    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) + "ns")
      result
    }

    // which one is faster switches back and forth
    // time { LinkedList.addOne(LinkedList(1 to 500:_*)) }
    // time { LinkedList.addOne2(LinkedList(1 to 500:_*)) }

  }

  "map" should "apply a function to each member in a list" in {
    val ll = LinkedList("a", "b", "c")
    val l2 = LinkedList(1, 2, 3)

    LinkedList.map(ll)(_.toUpperCase()) shouldEqual LinkedList("A", "B", "C")
    LinkedList.map(l2)(_.toString()) shouldEqual LinkedList("1", "2", "3")
  }

  "filter" should "remove the elements that fail a condition" in {
    val ll = LinkedList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    LinkedList.filter(ll)(_ % 2 == 0) shouldEqual LinkedList(2, 4, 6, 8, 10)
  }
}


