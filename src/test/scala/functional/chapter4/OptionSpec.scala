package functional.chapter4

import org.scalatest._

class OptionSpec extends FlatSpec with Matchers {
  "map" should "modify a value if present" in {
    val some: Maybe[Int] = Just(10)
    val none: Maybe[Int] = Nix

    some.map(_ * 10) shouldEqual Just(100)
    none.map(_ * 10) shouldEqual Nix
  }

  def div(n: Int, d: Int): Maybe[Int] = d match {
    case 0 => Nix
    case _ => Just(n / d)
  }

  "flatMap" should "return an option" in {
    val some: Maybe[Int] = Just(10)
    val none: Maybe[Int] = Nix

    some.flatMap(div(_, 10)) shouldEqual Just(1)
    some.flatMap(div(_, 0)) shouldEqual Nix
    none.flatMap(div(_, 10)) shouldEqual Nix
  }

  "getOrElse" should "return a default value if none" in {
    val some: Maybe[Int] = Just(10)
    val none: Maybe[Int] = Nix

    some.getOrElse(7) shouldEqual 10
    none.getOrElse(7) shouldEqual 7
  }

  "orElse" should "return a second option if the first is none" in {
    val some: Maybe[Int] = Just(10)
    val none: Maybe[Int] = Nix

    some.orElse(div(5, 0)) shouldEqual Just(10)
    some.orElse(div(5, 1)) shouldEqual Just(10)
    none.orElse(div(5, 0)) shouldEqual Nix
    none.orElse(div(5, 1)) shouldEqual Just(5)
  }

  "filter" should "return none if the vaule fails the predicate" in {
    val some: Maybe[Int] = Just(10)
    val none: Maybe[Int] = Nix

    some.filter(_ > 5) shouldEqual Just(10)
    some.filter(_ > 15) shouldEqual Nix
    none.filter(_ > 5) shouldEqual Nix

    some.filter2(_ > 5) shouldEqual Just(10)
    some.filter2(_ > 15) shouldEqual Nix
    none.filter2(_ > 5) shouldEqual Nix
  }

  "map2" should "combine two options into a new value" in {
    val s1 = Just(10)
    val s2 = Just(15)
    val n: Maybe[Int] = Nix

    Chapter4.map2(s1, s2)((a, b) => a + b) shouldEqual Just(25)
    Chapter4.map2(n, s2)((a, b) => a + b) shouldEqual Nix
    Chapter4.map2(s1, n)((a, b) => a + b) shouldEqual Nix
  }

  "sequence" should "return a list of all the values if they’re all defined" in {
    
  }
}
