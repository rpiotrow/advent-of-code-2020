package io.github.rpiotrow.advent2020.day11

trait Countable[F[_]] {
  def count[A](fa: F[A])(f: A => Boolean): Int
}

object Countable {

  def apply[F[_]](implicit c: Countable[F]): Countable[F] = c

  implicit class CountableOps[F[_], A](val fa: F[A]) extends AnyVal {
    def count(f: A => Boolean)(implicit c: Countable[F]): Int =
      c.count(fa)(f)
  }

}