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


  def explorePaths(pos: Pos, value: Int, acc: Set[Vector[Pos]]): Set[Vector[Pos]] =
    if grid(pos) == 9 then acc
    else {
      val next = pos.adjacentNeighbors.filter(grid.isInside).filter(grid(_) == value + 1)
      if next.isEmpty then acc
      else next.flatMap(n => explorePaths(n, value + 1, acc.map(_ :+ n)))
    }

  val result2 = startingPositions.flatMap(p => explorePaths(p, 0, Set(Vector(p)))).filter(_.size == 10)
  println(result2.size) // 1255