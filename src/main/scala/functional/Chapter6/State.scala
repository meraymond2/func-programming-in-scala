package functional.Chapter6

case class State[S,+A](run: S => (A,S)) {

  /* Exercise 11 */
  def unit[B >: A](a: B): State[S, B] =
    State(state => (a, state)) // or State(run)?

  def map[B](f: A => B): State[S, B] =
    State(state => {
      val (a, state2) = run(state)
      (f(a), state2)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(state => {
      val (a, state2) = run(state)
      f(a).run(state2)
    })

  /* Exercise 12 */
  // I have _no_ idea what he's talking about here. Skipping.
}

object State {

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    def loop(s: S, remaining: List[State[S, A]], res: List[A]): (List[A], S) = remaining match {
      case x :: xs =>
        val (a, s2) = x.run(s)
        loop(s2, xs, a +: res)
      case Nil =>
        (res, s)
    }
    State(state => loop(state, fs, List.empty[A]))
  }
}
