package io.github.rpiotrow.advent2020.day11

trait UnorderedFoldableWithInitValue[F[_]] {
  def unorderedFold[A, B](fa: F[A])(z: B)(f: (B, A) => B): B
}

object UnorderedFoldableWithInitValue {

  def apply[F[_]](implicit c: UnorderedFoldableWithInitValue[F]): UnorderedFoldableWithInitValue[F] = c

  implicit class UnorderedFoldableWithInitValueOps[F[_], A](val fa: F[A]) extends AnyVal {
    def unorderedFold[B](z: B)(f: (B, A) => B)(implicit c: UnorderedFoldableWithInitValue[F]): B =
      c.unorderedFold(fa)(z)(f)
  }

}