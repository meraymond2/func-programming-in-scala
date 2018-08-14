package functional.Chapter6

trait RNG {

  def nextInt: (Int, RNG)

}

object RNG {

  type Rand[+A] = RNG => (A, RNG)
//  type Rand[A] = State[RNG, A] -> doesn't compile

  /*
  This looked very confusing at first, but it's just:
    def int: Rand[Int] = rng => rng.nextInt

  That is, it takes a RNG, calls the next int method and return a
  tuple with the int and the new RNG. I could do the same with all of the
  functions below that don't take arguments.
   */
  val int: Rand[Int] = _.nextInt

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
    if (int == Integer.MIN_VALUE) {
      val (int2, rng3) = rng2.nextInt
      (int2, rng3)
    }
    else (int.abs, rng2)
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

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  /* Exercise 5 */
  def positiveMax(n: Int): Rand[Int] =
    map(positiveInt)(int => (int.toDouble / Integer.MAX_VALUE * n).toInt)

  /* Exercise 6 */
  def double2: Rand[Double] =
    map(positiveInt)(_.toDouble / Integer.MAX_VALUE)

  /* Exercise 7 */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def intDouble2: Rand[(Int, Double)] =
    map2(positiveInt, double2)((a, b) => (a, b))

  /* Exercise 8 */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def loop(rng: RNG, remaining: List[Rand[A]], res: List[A]): (List[A], RNG) = remaining match {
      case x :: xs =>
        val (a, rng2) = x(rng)
        loop(rng2, xs, a +: res)
      case Nil =>
        (res, rng)
    }
    rng => loop(rng, fs, List.empty[A])
  }

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  /* Exercise 9 */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def positiveInt2: Rand[Int] =
    flatMap(int)(a => {
      if (a == Integer.MIN_VALUE) int
      else {
        rng => (a.abs, rng)
      }
    })

}

