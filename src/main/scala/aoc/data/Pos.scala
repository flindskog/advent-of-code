package aoc.data

import scala.annotation.tailrec

case class Pos(row: Int, col: Int) {
  def move(direction: Direction): Pos = direction match {
    case Direction.Up    => Pos(row - 1, col)
    case Direction.Right => Pos(row, col + 1)
    case Direction.Down  => Pos(row + 1, col)
    case Direction.Left  => Pos(row, col - 1)
  }

  def move(direction: Direction, steps: Int): Pos =
    (0 until steps).foldLeft(this)((pos, _) => pos.move(direction))

  def manhattanDistance(other: Pos): Int =
    Math.abs(row - other.row) + Math.abs(col - other.col)

  def isInsideGrid[T](grid: Array[Array[T]]): Boolean =
    row >= 0 && row < grid.length && col >= 0 && col < grid(row).length

  def isOutsideGrid[T](grid: Array[Array[T]]): Boolean = !isInsideGrid(grid)

  def perpendicularNeighbors: Set[Pos] = Set(
    Pos(row - 1, col),
    Pos(row + 1, col),
    Pos(row, col - 1),
    Pos(row, col + 1)
  )

  def diagonalNeighbors: Set[Pos] = Set(
    Pos(row - 1, col - 1),
    Pos(row - 1, col + 1),
    Pos(row + 1, col - 1),
    Pos(row + 1, col + 1)
  )

  def neighbors = perpendicularNeighbors ++ diagonalNeighbors
}

object Pos {
  given Ordering[Pos] = Ordering.by(p => (p.row, p.col))

  val origo: Pos = Pos(0, 0)

  def drawAsGrid(positions: Set[Pos]): String = {
    val rows   = positions.map(_.row)
    val cols   = positions.map(_.col)
    val minRow = rows.min
    val maxRow = rows.max
    val minCol = cols.min
    val maxCol = cols.max

    (minRow to maxRow)
      .map(row =>
        (minCol to maxCol)
          .map(col => if (positions.contains(Pos(row, col))) "#" else ".")
          .mkString
      )
      .mkString("\n")
  }

  def floodFill(border: Set[Pos], start: Pos, max: Int): Option[Set[Pos]] = {
    @tailrec
    def loop(visited: Set[Pos], toVisit: Set[Pos]): Option[Set[Pos]] =
      if (toVisit.isEmpty) Some(visited)
      else if (visited.size > max) None
      else {
        val newVisited = visited ++ toVisit
        val newToVisit = toVisit
          .flatMap(_.neighbors)
          .diff(newVisited)
          .diff(border)
        loop(newVisited, newToVisit)
      }

    loop(Set.empty, Set(start)).map(_ ++ border)
  }

}
