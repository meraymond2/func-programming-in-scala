package functional.Chapter6

import org.scalatest._

/* Exercise 13 */
class MachineSpec extends FlatSpec with Matchers {

  /* Exercise 1 */
  "next" should "return a new machine with updated state" in {

    /* Initial */
    val machine = Machine(locked = true, candies = 15, coins = 0)
    machine.locked shouldBe true
    machine.coins shouldEqual 0
    machine.candies shouldEqual 15

    /* Enter a coin */
    val (c1, m1) = Machine.next(Coin).run(machine)
    c1 shouldEqual 1
    m1.locked shouldBe false
    m1.candies shouldEqual 15

    /* Enter a second coin - nothing happens */
    val (c2, m2) = Machine.next(Coin).run(m1)
    c2 shouldEqual 1
    m2.locked shouldBe false
    m2.candies shouldEqual 15

    /* Turn the handle, get a candy */
    val (c3, m3) = Machine.next(Turn).run(m2)
    c3 shouldEqual 1
    m3.locked shouldBe true
    m3.candies shouldEqual 14

    /* Try to turn again, nothing happens */
    val (c4, m4) = Machine.next(Turn).run(m3)
    c4 shouldEqual 1
    m4.locked shouldBe true
    m4.candies shouldEqual 14

  }

  "simulateMachine" should "return a new machine after processing multiple inputs" in {
    val machine = Machine(locked = true, candies = 5, coins = 0)

    val (c, m) = Machine.simulateMachine(List(
      Coin,
      Turn,
      Coin,
      Turn,
      Turn,
      Coin,
      Coin,
      Turn,
      Coin,
      Turn,
      Coin,
      Turn,
      Turn,
      Coin,
      Turn,
      Coin
    )).run(machine)

    c shouldEqual 5
    m.locked shouldBe true
    m.candies shouldEqual 0

    val (c2, m2) = Machine.simulateMachine(List(
      Coin, Turn, Coin, Turn, Turn
    )).run(machine)

    c2 shouldEqual 2
    m2.locked shouldBe true
    m2.candies shouldEqual 3
  }

}
