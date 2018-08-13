package functional.Chapter6

trait RNG {

  def nextInt: (Int, RNG)

}

object RNG {

  def simple(seed: Long): RNG = new RNG {
    def nextInt: (Int, RNG) = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }

  /* Exercise 1 */
  def positiveInt(rng: RNG): (Int, RNG) = {
    val (int, rng2) = rng.nextInt
    val positive = if (int == Integer.MIN_VALUE) (int + 1).abs else int.abs
    (positive, rng2)
  }

  /* Exercise 2 */
  def double(rng: RNG): (Double, RNG) = {
    val (int, rng2) = RNG.positiveInt(rng)
    val double = int / Integer.MAX_VALUE.toDouble
    (double, rng2)
  }

  /* Exercise 3 */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, rng2) = RNG.positiveInt(rng)
    val (double, rng3) = RNG.double(rng2)
    ((int, double), rng3)
  }

  // skipping doubleInt and double3, cause they're the same thing

  /* Exercise 4 */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(rng1: RNG, list: List[Int], i: Int): (List[Int], RNG) = {
      if (i < count) {
        val (int, rng2) = rng1.nextInt
        loop(rng2, int +: list, i + 1)
      }
      else (list, rng1)
    }
    loop(rng, List.empty[Int], 0)
  }

}
