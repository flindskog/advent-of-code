package aoc.syntax

import aoc.utils.SeqUtils

trait SeqSyntax {
  extension [T](seq: Seq[Seq[T]]) {
    def rotateRight: Seq[Seq[T]] = SeqUtils.rotateRight(seq)
    def rotateLeft: Seq[Seq[T]]  = SeqUtils.rotateLeft(seq)
  }
}
