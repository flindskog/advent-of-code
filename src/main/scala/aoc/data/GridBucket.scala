package aoc.data

import scala.annotation.targetName

class GridBucket(cellSize: Int)(private val elements: Set[Pos]) {
  // Buckets for storing points, keyed by grid coordinates
  private val buckets: Map[(Int, Int), Set[Pos]] = elements.groupBy(getCell)

  // Helper method to calculate grid cell coordinates
  private def getCell(pos: Pos): (Int, Int) =
    (pos.row / cellSize, pos.col / cellSize)

  // Add a position to the grid
  def add(pos: Pos): GridBucket = new GridBucket(cellSize)(elements + pos)

  @targetName("plus")
  def +(pos: Pos): GridBucket = add(pos)

  def remove(pos: Pos): GridBucket = new GridBucket(cellSize)(elements - pos)

  @targetName("minus")
  def -(pos: Pos): GridBucket = remove(pos)

  // Find all points within Manhattan distance `distance` of a given point
  def findWithinDistance(pos: Pos, distance: Int): Seq[Pos] = {
    val (centerRow, centerCol) = getCell(pos)

    val (fromRow, fromCol) = getCell(Pos(pos.row - distance, pos.col - distance))
    val (toRow, toCol)     = getCell(Pos(pos.row + distance, pos.col + distance))

    val rowRange = fromRow to toRow
    val colRange = fromCol to toCol

    // Check all neighboring cells
    for {
      row    <- rowRange
      col    <- colRange
      bucket <- buckets.get((row, col)).toSeq
      point  <- bucket
      if point.manhattanDistance(pos) <= distance
    } yield point
  }
}
