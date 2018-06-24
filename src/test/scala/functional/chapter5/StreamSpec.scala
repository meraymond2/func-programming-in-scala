package functional.chapter5
import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {

  "toList" should "return a list" in {
    val ll = LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    ll.toList shouldEqual List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    val empty: LazyList[String] = LazyList()
    empty.toList shouldEqual Nil
  }

}
