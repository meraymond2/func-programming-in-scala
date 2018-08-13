package functional.Chapter6

// Changing this to a class, his example with an object doesn't compile
class RNG(seed: Long) {

  def nextInt: (Int, RNG) = {
    val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    ((seed2 >>> 16).asInstanceOf[Int], new RNG(seed2))
  }

  /* Exercise 1 */
  def positiveInt: (Int, RNG) = {
    val (int, rng) = nextInt
    val positive = if (int == Integer.MIN_VALUE) (int + 1).abs else int.abs
    (positive, rng)
  }

  /* Exercise 2 */
  def double: (Double, RNG) = {
    val (int, rng) = positiveInt
    val double = int / Integer.MAX_VALUE.toDouble
    (double, rng)
  }

  /* Exercise 3 */
  def intDouble: ((Int, Double), RNG) = {
    val (int, rng2) = positiveInt
    val (double, rng3) = rng2.double
    ((int, double), rng3)
  }

  // skipping doubleInt and double3, cause they're the same thing

  /* Exercise 4 */
  def ints(count: Int): (List[Int], RNG) = {
    def loop(rng: RNG, list: List[Int], i: Int): (List[Int], RNG) = {
      if (i < count) {
        val (int, rng2) = rng.nextInt
        loop(rng2, int +: list, i + 1)
      }
      else (list, rng)
    }
    loop(this, List.empty[Int], 0)
  }

}
