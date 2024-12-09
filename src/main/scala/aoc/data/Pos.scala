package aoc.data

import scala.annotation.{tailrec, targetName}

case class Pos(row: Int, col: Int) {
  def move(direction: Direction): Pos = direction match {
    case Direction.Up    => Pos(row - 1, col)
    case Direction.Right => Pos(row, col + 1)
    case Direction.Down  => Pos(row + 1, col)
    case Direction.Left  => Pos(row, col - 1)
  }

  def move(direction: Direction, steps: Int): Pos =
    (1 to steps).foldLeft(this)((pos, _) => pos.move(direction))

  def manhattanDistance(other: Pos): Int =
    Math.abs(row - other.row) + Math.abs(col - other.col)

  def isTouching(other: Pos): Boolean = neighbors.contains(other)

  def isInsideGrid[T](grid: Array[Array[T]]): Boolean =
    row >= 0 && row < grid.length && col >= 0 && col < grid(row).length

  def isOutsideGrid[T](grid: Array[Array[T]]): Boolean = !isInsideGrid(grid)

  def adjacentNeighbors: Set[Pos] = Set(
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

  def neighbors = adjacentNeighbors ++ diagonalNeighbors

  @targetName("plusPos")
  def +(other: Pos): Pos = Pos(row + other.row, col + other.col)

  @targetName("minusPos")
  def -(other: Pos): Pos = Pos(row - other.row, col - other.col)

  @targetName("unaryMinus")
  def unary_- : Pos = Pos(-row, -col)

  def addRow(row: Int): Pos = Pos(this.row + row, this.col)

  def addCol(col: Int): Pos = Pos(this.row, this.col + col)
}

object Pos {
  given Ordering[Pos] = Ordering.by(p => (p.row, p.col))

  def apply(p: (Int, Int)): Pos = Pos(p._1, p._2)

  val origo: Pos = Pos(0, 0)

  def drawAsGrid(positions: Set[Pos]): String = drawAsGrid(Map('#' -> positions))

  def drawAsGrid(positions: Set[Pos], minRow: Int, maxRow: Int, minCol: Int, maxCol: Int): String =
    drawAsGrid(Map('#' -> positions), minRow: Int, maxRow: Int, minCol: Int, maxCol: Int)

  def drawAsGrid(objects: Map[Char, Set[Pos]]): String = {
    val rows   = objects.values.flatMap(_.map(_.row))
    val cols   = objects.values.flatMap(_.map(_.col))
    val minRow = rows.min
    val maxRow = rows.max
    val minCol = cols.min
    val maxCol = cols.max
    drawAsGrid(objects, minRow, maxRow, minCol, maxCol)
  }

  def drawAsGrid(objects: Map[Char, Set[Pos]], minRow: Int, maxRow: Int, minCol: Int, maxCol: Int): String =
    (minRow to maxRow)
      .map(row =>
        (minCol to maxCol)
          .map(col =>
            objects.find { case (_, positions) => positions.contains(Pos(row, col)) }
              .map(_._1)
              .getOrElse('.')
          )
          .mkString
      )
      .mkString("\n")

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
