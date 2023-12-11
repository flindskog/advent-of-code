package aoc.y2023

object Day11 extends Aoc2023("input_11.txt"):

  case class Position(row: Int, col: Int) {
    def distance(other: Position): Int =
      val dx = math.abs(other.row - row)
      val dy = math.abs(other.col - col)
      dy + dx
  }

  extension (i: Int) def isBetween(a: Int, b: Int): Boolean = (i >= a && i <= b) || (i >= b && i <= a)

  val empty = '.'

  val universe: Array[Array[Char]] = input.map(_.toCharArray).toArray

  val emptyRows = universe.zipWithIndex.filter((row, _) => row.forall(_ == empty)).map((_, i) => i).toSet
  val emptyCols = universe.transpose.zipWithIndex.filter((col, _) => col.forall(_ == empty)).map((_, i) => i).toSet

  val galaxies = universe.indices.flatMap { row =>
    universe(row).indices.flatMap { col =>
      val cell = universe(row)(col)
      if cell == empty then None
      else Some((row, col))
    }
  }

  val positionPairs = galaxies
    .combinations(2)
    .map { case IndexedSeq((r1, c1), (r2, c2)) =>
      (Position(r1, c1), Position(r2, c2))
    }
    .toList

  def expandedDistances(factor: Long) = positionPairs.map { case (p1, p2) =>
    val unexpandedDistance = p1.distance(p2)

    val eR = emptyRows.count(row => row.isBetween(p1.row, p2.row))
    val eC = emptyCols.count(col => col.isBetween(p1.col, p2.col))
    (eR + eC) * (factor - 1) + unexpandedDistance
  }

  println(expandedDistances(2).sum)       // 9627977
  println(expandedDistances(1000000).sum) // 644248339497
