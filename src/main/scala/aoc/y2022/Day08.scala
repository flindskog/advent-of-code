package aoc.y2022

import scala.annotation.tailrec

object Day08 extends Aoc2022("input_08.txt"):

  val grid = input.toGrid(_.asDigit)

  def look(deltaRow: Int, deltaCol: Int, row: Int, col: Int): Set[(Int, Int)] = {
    @tailrec
    def loop(row: Int, col: Int, acc: Set[(Int, Int)], curHeight: Int): Set[(Int, Int)] =
      if row < 0 || col < 0 || row >= grid.length || col >= grid(row).length then acc
      else if grid(row)(col) > curHeight then loop(row + deltaRow, col + deltaCol, acc + ((row, col)), grid(row)(col))
      else loop(row + deltaRow, col + deltaCol, acc, curHeight)

    loop(row, col, Set.empty, 0)
  }

  val rows = grid.length
  val cols = grid.head.length

  val fromLeft   = (0 until rows).flatMap(row => look(0, 1, row, 0))
  val fromRight  = (0 until rows).flatMap(row => look(0, -1, row, cols - 1))
  val fromTop    = (0 until cols).flatMap(col => look(1, 0, 0, col))
  val fromBottom = (0 until cols).flatMap(col => look(-1, 0, rows - 1, col))

  val result = (fromLeft ++ fromRight ++ fromTop ++ fromBottom).toSet.size

  println(result)

  def view(deltaRow: Int, deltaCol: Int, row: Int, col: Int): Int = {
    val curHeight = grid(row)(col)
    @tailrec
    def loop(row: Int, col: Int, acc: Int): Int =
      if row < 0 || col < 0 || row >= grid.length || col >= grid(row).length then acc
      else if grid(row)(col) < curHeight then loop(row + deltaRow, col + deltaCol, acc + 1)
      else acc + 1

    loop(row + deltaRow, col + deltaCol, 0)
  }

  val result2 = grid.indices.map { row =>
    grid(row).indices.map { col =>
      view(0, 1, row, col) * view(0, -1, row, col) * view(1, 0, row, col) * view(-1, 0, row, col)
    }.max
  }.max

  println(result2)