package aoc.utils

import aoc.data.Pos

object Polygon {
  /**
   * Calculates the area of a polygon given a list of positions
   * using the shoelace formula
   * https://en.wikipedia.org/wiki/Shoelace_formula
   */
  def areaOf(positions: Seq[Pos]): Long = {
    val shoelace = (positions.last +: positions)
      .zip(positions)
      .map { case (a, b) =>
        a.col.toLong * b.row.toLong - b.col.toLong * a.row.toLong
      }
    math.abs(shoelace.sum) / 2L
  }
}
