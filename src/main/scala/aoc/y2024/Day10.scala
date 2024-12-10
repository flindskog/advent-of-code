package aoc.y2024

import aoc.data.Pos

object Day10 extends Aoc2024("input_10.txt"):
  val grid = input.toGrid(_.asDigit)

  val startingPositions = grid.findAll(0)

  def explore(pos: Pos, value: Int, acc: Set[Pos]): Set[Pos] =
    if grid(pos) == 9 then acc + pos
    else {
      val next = pos.adjacentNeighbors.filter(grid.isInside).filter(grid(_) == value + 1)
      if next.isEmpty then acc
      else next.flatMap(n => explore(n, value + 1, acc))
    }

  val result = startingPositions.flatMap(p => explore(p, 0, Set.empty))

  println(result.size) // 550
