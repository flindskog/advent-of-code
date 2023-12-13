package aoc.syntax

import aoc.utils.Matrix

trait SeqSyntax {
  extension [T](seq: Seq[Seq[T]]) {
    def rotateRight: Seq[Seq[T]] = Matrix.rotateRight(seq)
    def rotateLeft: Seq[Seq[T]]  = Matrix.rotateLeft(seq)
  }
}
