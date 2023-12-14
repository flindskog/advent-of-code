package aoc.y2023

import aoc.utils.ArrayUtils

import scala.annotation.tailrec

object Day14 extends Aoc2023("input_14.txt"):
  private val round  = 'O'
  private val square = '#'

  type Grid = Array[Array[Char]]

  def tilt(matrix: Grid): Grid = {
    val height = matrix.length

    def tiltColumn(col: Int): Unit = {
      def isRound(row: Int, col: Int): Boolean = matrix(row)(col) == round

      def isSquare(row: Int, col: Int): Boolean = matrix(row)(col) == square

      // Swaps the content between row1 and row2
      def swap(row1: Int, row2: Int): Unit = {
        val tmp = matrix(row1)(col)
        matrix(row1)(col) = matrix(row2)(col)
        matrix(row2)(col) = tmp
      }

      @tailrec
      def tilt0(lower: Int, upper: Int): Unit =
        if (upper <= -1 || lower <= 0) // Reached the top, we're done
          ()
        else if (isRound(lower, col) || isSquare(lower, col)) // flake at the lower pointer, move both pointers up
          tilt0(lower - 1, lower - 2)
        else if (isRound(upper, col)) { // flake at the upper pointer, move it to the lower pointer
          swap(upper, lower)
          tilt0(lower - 1, upper - 1)
        } else if (isSquare(upper, col)) {
          tilt0(upper - 1, upper - 2)
        } else // No flake at the upper or lower pointer, move the upper pointer up
          tilt0(lower, upper - 1)

      tilt0(height - 1, height - 2)
    }

    matrix.head.indices.foreach(tiltColumn)
    matrix
  }

  def calculateNorthLoad(matrix: Grid): Int =
    matrix.head.indices.map { col =>
      matrix.indices.map { row =>
        if matrix(row)(col) == round then matrix.length - row else 0
      }.sum
    }.sum

  def cycle(grid: Grid): Grid = {
    // North
    tilt(ArrayUtils.inPlaceRotate90Degrees(grid))

    // West
    ArrayUtils.inPlaceRotateRight(grid)
    tilt(grid)

    // South
    ArrayUtils.inPlaceRotateRight(grid)
    tilt(grid)

    // East
    ArrayUtils.inPlaceRotateRight(grid)
    tilt(grid)

    ArrayUtils.inPlaceRotateLeft(grid)
  }

  def detectCycles(l: List[String]): Option[Int] =
    if l.size < 2 then None
    else {
      (1 to l.size / 2 flatMap { i =>
        val (a, b) = l.splitAt(i)
        if b.startsWith(a) then Some(a.size)
        else None
      }).toList.headOption
    }

  def tryPredict(stack: List[(Int, String, Int)], afterCycles: Int): Option[Int] =
    detectCycles(stack.map(_._2)).map { cycleSize =>
      val cycleNo     = stack.head._1
      val index       = cycleSize - ((afterCycles - cycleNo) % cycleSize)
      val loadAtCycle = stack(index)._3
      loadAtCycle
    }

  @tailrec
  def predictLoad(grid: Grid, afterCycles: Int, cycleNo: Int = 1, stack: List[(Int, String, Int)] = Nil): Int = {
    cycle(grid)
    val load      = calculateNorthLoad(grid)
    val gridState = grid.map(_.mkString).mkString
    val newStack  = (cycleNo, gridState, load) :: stack
    tryPredict(newStack, afterCycles) match {
      case Some(load) => load
      case None       => predictLoad(grid, afterCycles, cycleNo + 1, newStack)
    }
  }

  val res = input.splitByEmptyLine.map(_.map(_.toCharArray).toArray).map { grid =>
    ArrayUtils.inPlaceRotate90Degrees(tilt(ArrayUtils.inPlaceRotate90Degrees(grid)))
  }

  res.map(calculateNorthLoad).foreach(println) // 108918

  val res2 = input.splitByEmptyLine.map(_.map(_.toCharArray).toArray).map { grid =>
    predictLoad(grid, 1_000_000_000)
  }

  res2.foreach(println) // 100310
