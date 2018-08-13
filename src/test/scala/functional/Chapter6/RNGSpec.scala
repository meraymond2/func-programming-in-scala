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
    val rng = RNG.simple(seed)

    val (int, _) = RNG.positiveMax(1000)(rng)

    int shouldEqual 115

  }

  /* Exercise 6 */
  "double2" should "also return a pseudo-random number between 0 and 1" in {
    val rng = RNG.simple(seed)

    val (double, rng2) = RNG.double2(rng)
    val (double2, _) = RNG.double2(rng2)

    double shouldEqual 0.11538987938100001d
    double2 shouldEqual 0.25709313259324673d

  }

  /* Exercise 7 */
  "intDouble2" should "also return a tuple with an int and a double" in {
    val rng = RNG.simple(seed)

    val ((int, double), _) = RNG.intDouble2(rng)

    int shouldEqual 247797879
    double shouldEqual 0.25709313259324673d
  }

  /* Exercise 8 */
  "ints2" should "also return a list of length n of psuedo-random integers" in {
    val rng = RNG.simple(seed)

    val (ints, rng2) = RNG.ints2(5)(rng)
    val (ints2, _) = RNG.ints2(6)(rng)

    ints shouldEqual List(149386600, 1840538571, -2068089078, -552103298, -247797879)
    ints2 shouldEqual List(-2063692454, 149386600, 1840538571, -2068089078, -552103298, -247797879)
  }

  /* Exercise 9 */
  "positiveInt2" should "also return a pseudo-random positive integer" in {

    val rng = RNG.simple(seed)

    val (int, rng2) = RNG.positiveInt2(rng)
    val (int2, _) = RNG.positiveInt2(rng)

    int shouldEqual 247797879
    int2 shouldEqual 247797879

    val (int3, _) = RNG.positiveInt2(rng2)

    int3 should not equal 247797879

  }

  /* Exercise 11 */
  "State" should "have all the functionality of RNG, generalised" in {

    val rng = RNG.simple(seed)
    val IntGen = State[RNG, Int](_.nextInt)

    val (int, _) = IntGen.run(rng)
    int shouldEqual -247797879

    val (mapped, _) = IntGen.map(_.abs).run(rng)
    mapped shouldEqual 247797879

    val (flatMapped, _): ((Int, Int), RNG) = IntGen.flatMap(int =>
      State(rng => {
        val (int2, rng2) = rng.nextInt
        ((int, int2), rng2)
      })
    ).run(rng)

    flatMapped shouldEqual (-247797879, -552103298)

    val (sequence, _) = State.sequence(List.fill(5)(IntGen)).run(rng)
    sequence shouldEqual List(149386600, 1840538571, -2068089078, -552103298, -247797879)

  }

}
