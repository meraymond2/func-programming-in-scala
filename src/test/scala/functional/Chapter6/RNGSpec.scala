package functional.Chapter6

import org.scalatest._

class RNGSpec extends FlatSpec with Matchers {

  val seed = 12345678L

  /* Exercise 1 */
  "positive Int" should "return a pseudo-random positive integer" in {

    val rng = RNG.simple(seed)

    val (int, rng2) = RNG.positiveInt(rng)
    val (int2, _) = RNG.positiveInt(rng)

    int shouldEqual 247797879
    int2 shouldEqual 247797879

    val (int3, _) = RNG.positiveInt(rng2)

    int3 should not equal 247797879

  }

  /* Exercise 2 */
  "double" should "return a pseudo-random number between 0 and 1" in {
    val rng = RNG.simple(seed)

    val (double, rng2) = RNG.double(rng)
    val (double2, _) = RNG.double(rng2)

    double shouldEqual 0.11538987938100001d
    double2 shouldEqual 0.25709313259324673d

  }

  /* Exercise 3 */
  "intDouble" should "return a tuple with an int and a double" in {
    val rng = RNG.simple(seed)

    val ((int, double), _) = RNG.intDouble(rng)

    int shouldEqual 247797879
    double shouldEqual 0.25709313259324673d
  }

  /* Exercise 4 */
  "ints" should "return a list of length n of psuedo-random integers" in {
    val rng = RNG.simple(seed)

    val (ints, rng2) = RNG.ints(5)(rng)
    val (ints2, _) = RNG.ints(6)(rng)

    ints shouldEqual List(149386600, 1840538571, -2068089078, -552103298, -247797879)
    ints2 shouldEqual List(-2063692454, 149386600, 1840538571, -2068089078, -552103298, -247797879)
  }

  /* Exercise 5 */
  "positiveMax" should "return a pseudo random number between 0 and n" in {

  }

}
