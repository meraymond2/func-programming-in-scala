package functional.chapter5
import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {

  "toList" should "return a list" in {
    val ll = LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    ll.toList shouldEqual List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    val empty: LazyList[String] = LazyList()
    empty.toList shouldEqual Nil
  }

  "take" should "return a new lazylist with n number of elements" in {
    val ll = LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    ll.take(3).toList shouldEqual LazyList(1, 2, 3).toList

    ll.take(15).toList shouldEqual ll.toList

    ll.take(0).toList shouldEqual List.empty[Int]
  }

  "takeWhile" should "take elements until an element fails the predicate" in {
    val ll = LazyList("string", "object", "stream", "int", "double", "option")

    ll.takeWhile(_.length > 3).toList shouldEqual List("string", "object", "stream")
  }

  "forall" should "check whether every element in a stream satifies a predicate" in {
    val ll = LazyList(88, 30, 99, 72)

    ll.forAll(_ > 50) shouldBe false
    ll.forAll(_ > 25) shouldBe true
  }

  "takeWhile2" should "take elements until an element fails the predicate" in {
    val ll = LazyList("string", "object", "stream", "int", "double", "option")

    ll.takeWhile2(_.length > 3).toList shouldEqual List("string", "object", "stream")
  }

  "map" should "return a new stream with a function applied to eac element" in {
    val ll = LazyList(1, 2, 3, 4, 5)

    ll.map(_ * 10).toList shouldEqual List(10, 20, 30, 40, 50)
  }

  "filter" should "return a stream without any elements that fail the predicate" in {
    val ll = LazyList("string", "object", "stream", "int", "double", "option")

    ll.filter(_.length > 3).toList shouldEqual List("string", "object", "stream", "double", "option")
  }

  "append" should "attach an element to the end of the stream" in {
    val ll = LazyList(1, 2, 3)

    ll.append(4).toList shouldEqual List(1, 2, 3, 4)
  }

  "flatMap" should "return a new stream with a function applied to each element, and flattened" in {
    val ll = LazyList("Cascat", "Luna", "Sherlock")

    ll.flatMap(str => LazyList(str.toCharArray:_*)).toList shouldEqual List('C', 'a', 's', 'c', 'a', 't', 'L', 'u', 'n', 'a', 'S', 'h', 'e', 'r', 'l', 'o', 'c', 'k')
  }

  "constant" should "return an infinite stream of a given value" in {
    val infinite = LazyList.constant("Cascat")

    infinite.take(3).toList shouldEqual List("Cascat", "Cascat", "Cascat")
  }

  "from" should "retrn an infinite series of integers, starting a n" in {
    val infinite = LazyList.from(5)

    infinite.take(5).toList shouldEqual List(5, 6, 7, 8, 9)
  }

  "fibs" should "return an infinite stream of Fibonacci numbers" in {
    val fibs = LazyList.fibs

    fibs.take(10).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  }

  "unfold" should "create a stream from a start point and a function" in {
    val ll = LazyList.unfold(0)(i => {
      if (i <= 10) Some(i, i + 2) else None
    })

    ll.toList shouldEqual List(0, 2, 4, 6, 8, 10)

    val fibs = LazyList.fibs2
    fibs.take(10).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)

    val from = LazyList.from2(5)
    from.take(5).toList shouldEqual List(5, 6, 7, 8, 9)

    val constants = LazyList.constant2("Cascat")
    constants.take(3).toList shouldEqual List("Cascat", "Cascat", "Cascat")

    val twos = LazyList.twos
    twos.take(12).toList shouldEqual List(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
  }

}
