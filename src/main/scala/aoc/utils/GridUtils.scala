package aoc.utils

import aoc.data.Pos

object GridUtils {
  def inPlaceRotateRight[T](grid: Array[Array[T]]): Array[Array[T]] = {
    if (grid.nonEmpty && grid.forall(_.nonEmpty)) {
      val numRows = grid.length
      val numCols = grid(0).length

      for {
        layer <- 0 until numRows / 2
        i     <- layer until numCols - 1 - layer
      } {
        val temp = grid(layer)(i)
        grid(layer)(i) = grid(numRows - 1 - i)(layer)
        grid(numRows - 1 - i)(layer) = grid(numRows - 1 - layer)(numCols - 1 - i)
        grid(numRows - 1 - layer)(numCols - 1 - i) = grid(i)(numCols - 1 - layer)
        grid(i)(numCols - 1 - layer) = temp
      }
    }
    grid
  }

  def inPlaceRotateLeft[T](grid: Array[Array[T]]): Array[Array[T]] = {
    if (grid.nonEmpty && grid.forall(_.nonEmpty)) {
      val numRows = grid.length
      val numCols = grid(0).length

      for {
        layer <- 0 until numRows / 2
        i     <- layer until numCols - 1 - layer
      } {
        val temp = grid(layer)(i)
        grid(layer)(i) = grid(i)(numCols - 1 - layer)
        grid(i)(numCols - 1 - layer) = grid(numRows - 1 - layer)(numCols - 1 - i)
        grid(numRows - 1 - layer)(numCols - 1 - i) = grid(numRows - 1 - i)(layer)
        grid(numRows - 1 - i)(layer) = temp
      }
    }
    grid
  }

  def inPlaceRotate90Degrees[T](m: Array[Array[T]]): Array[Array[T]] =
    inPlaceRotateRight(inPlaceRotateRight(m))

  def prettyPrint[T](grid: Array[Array[T]]): String =
    grid.map(_.mkString).mkString("\n")

  def find[T](grid: Array[Array[T]], value: T): Option[Pos] =
    findAll(grid, value).headOption

  def findAll[T](grid: Array[Array[T]], value: T): Seq[Pos] = {
    val numRows = grid.length
    val numCols = grid(0).length

    for {
      row <- 0 until numRows
      col <- 0 until numCols
      if grid(row)(col) == value
    } yield Pos(row, col)
  }

  def isInsideGrid[T](grid: Array[Array[T]], pos: Pos): Boolean =
    pos.row >= 0 && pos.row < grid.length && pos.col >= 0 && pos.col < grid(pos.row).length

  def toPosTuples[T](grid: Array[Array[T]]): Array[(Pos, T)] =
    grid.zipWithIndex.flatMap { case (row, rowIdx) =>
      row.zipWithIndex.map { case (cell, colIdx) =>
        Pos(rowIdx, colIdx) -> cell
      }
    }
}
