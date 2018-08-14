package functional.Chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

/* Exercise 13 */
object Machine {
  type Coins = Int

  def next(input: Input): State[Machine, Coins] =
    State(m => input match {
      case Coin =>
        if (m.locked && m.candies > 0) {
          val nextCoins = m.coins + 1
          (nextCoins, m.copy(locked = false, coins = nextCoins))
        }
        else (m.coins, m)
      case Turn =>
        if (!m.locked && m.candies > 0) {
          (m.coins, m.copy(locked = true, candies = m.candies - 1))
        }
        else (m.coins, m)
    })


  def simulateMachine(inputs: List[Input]): State[Machine, Coins] =
    State(m => {
      val m3 = inputs.foldLeft(m)((z, input) => {
        val (_, m2) = next(input).run(z)
        m2
      })
      (m3.coins, m3)
    })

}
