package functional.Chapter7

import java.util.concurrent.{Callable, ExecutorService, Future}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] =
    a(s)

  /* Exercise 3 */
  def unit[A](a: A): Par[A] =
    s => s.submit(() => a)

}

