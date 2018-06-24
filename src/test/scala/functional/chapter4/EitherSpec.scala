package functional.chapter4

import org.scalatest._

class EitherSpec extends FlatSpec with Matchers {
  "map" should "modify a value if is right" in {
    val left: Result[String, String] = Bad("I’m an error message.")
    val right: Result[String, String] = Good("I’m the value that we wanted.")

    left.map(_.length) shouldEqual left
    right.map(_.length) shouldEqual Good(29)
  }

  val left: Result[String, Int] = Bad("I’m an error message.")
  val right: Result[String, Int] = Good(7)

  "flatMap" should "flatten a nested Either and modify a value" in {
    left.flatMap(n => Chapter4Either.safeDiv(n, 10)) shouldEqual left
    right.flatMap(n => Chapter4Either.safeDiv(n, 7)) shouldEqual Good(1)
    right.flatMap(n => Chapter4Either.safeDiv(n, 0)) shouldEqual Bad("Divide by 0 error.")
  }

  "orElse" should "return a new value if left" in {
    left.orElse(Chapter4Either.safeDiv(7, 7)) shouldEqual Good(1)
    left.orElse(Chapter4Either.safeDiv(7, 0)) shouldEqual Bad("Divide by 0 error.")
    right.orElse(Chapter4Either.safeDiv(7, 0)) shouldEqual Good(7)
  }

  "map2" should "combine to eithers together" in {
    left.map2(right)((x, y) => x + y) shouldEqual Bad("I’m an error message.")
    Good(10).map2(right)((x, y) => x + y) shouldEqual Good(17)
  }

}
