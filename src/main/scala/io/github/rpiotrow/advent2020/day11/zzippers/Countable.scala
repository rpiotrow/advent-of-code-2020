package io.github.rpiotrow.advent2020.day11.zzippers

import zio.UIO

trait Countable[F[_]] {
  def count[A](fa: F[A])(f: A => Boolean): UIO[Long]
}

object Countable {

  def apply[F[_]](implicit c: Countable[F]): Countable[F] = c

  implicit class CountableOps[F[_], A](val fa: F[A]) extends AnyVal {
    def count(f: A => Boolean)(implicit c: Countable[F]): UIO[Long] =
      c.count(fa)(f)
  }

}