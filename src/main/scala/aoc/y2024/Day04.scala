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

  println(result) // 2378

  def searchCross(grid: Grid[Char], row: Int, col: Int): Int = {
    val above = ((-1, -1), (-1, 1))
    val below = ((1, -1), (1, 1))
    val left = ((-1, -1), (1, -1))
    val right = ((-1, 1), (1, 1))

    def isChar(char: Char)(d: ((Int, Int), (Int, Int))): Boolean = {
      val (r1, c1) = d._1
      val (r2, c2) = d._2
      grid(row + r1)(col + c1) == char && grid(row + r2)(col + c2) == char
    }

    val isM = isChar('M')
    val isS = isChar('S')

    Try {
      if (grid(row)(col) == 'A' &&
        ((isM(above) && isS(below)) ||
        (isM(below) && isS(above)) ||
        (isM(left) && isS(right)) ||
        (isM(right) && isS(left)))) 1
      else 0
    }.getOrElse(0)
  }

  val resultCross = grid.indices.map { row =>
    grid(row).indices.map { col =>
      searchCross(grid, row, col)
    }.sum
  }.sum

  println(resultCross) // 1796