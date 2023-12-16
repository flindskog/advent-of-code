package aoc.syntax

import aoc.utils.GridUtils

trait GridSyntax {
  extension[T] (grid: Array[Array[T]]) {
    def prettyPrint: String = GridUtils.prettyPrint(grid)
  }
}
