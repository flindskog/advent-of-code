package aoc.y2024

import scala.util.Try

object Day04 extends Aoc2024("input_04.txt"):
  val grid: Grid[Char] = input.toGrid()

  def search(grid: Grid[Char], row: Int, col: Int): Int = {
    val horRight      = ((0, 1), (0, 2), (0, 3))
    val horLeft       = ((0, -1), (0, -2), (0, -3))
    val verUp         = ((-1, 0), (-2, 0), (-3, 0))
    val verDown       = ((1, 0), (2, 0), (3, 0))
    val diagUpRight   = ((-1, 1), (-2, 2), (-3, 3))
    val diagUpLeft    = ((-1, -1), (-2, -2), (-3, -3))
    val diagDownRight = ((1, 1), (2, 2), (3, 3))
    val diagDownLeft  = ((1, -1), (2, -2), (3, -3))

    val directions = List(horRight, horLeft, verUp, verDown, diagUpRight, diagUpLeft, diagDownRight, diagDownLeft)

    directions.map { (d1, d2, d3) =>
      Try {
        if (
          grid(row)(col) == 'X' && grid(row + d1._1)(col + d1._2) == 'M' && grid(row + d2._1)(
            col + d2._2
          ) == 'A' && grid(row + d3._1)(col + d3._2) == 'S'
        ) 1
        else 0
      }.getOrElse(0)
    }.sum
  }

  val result = grid.indices.map { row =>
    grid(row).indices.map { col =>
      search(grid, row, col)
    }.sum
  }.sum

  println(result)
