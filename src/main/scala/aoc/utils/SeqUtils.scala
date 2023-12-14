package aoc.utils

object SeqUtils {
  def rotateRight[T](m: Seq[Seq[T]]): Seq[Seq[T]] =
    m.transpose.map(_.reverse)

  def rotateLeft[T](m: Seq[Seq[T]]): Seq[Seq[T]] =
    m.transpose.reverse
}
