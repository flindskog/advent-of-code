package aoc.syntax

import aoc.data.Pos
import aoc.utils.GridUtils

trait GridSyntax {
  extension [T](grid: Array[Array[T]]) {
    def prettyPrint: String          = GridUtils.prettyPrint(grid)
    def find(t: T): Option[Pos]      = GridUtils.find(grid, t)
    def findAll(t: T): Seq[Pos]      = GridUtils.findAll(grid, t)
    def isInside(pos: Pos): Boolean  = GridUtils.isInsideGrid(grid, pos)
    def toPosTuples: Array[(Pos, T)] = GridUtils.toPosTuples(grid)
    def apply(pos: Pos): T           = grid(pos.row)(pos.col)
  }
}
