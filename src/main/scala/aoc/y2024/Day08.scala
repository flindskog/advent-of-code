package aoc.y2024

import aoc.data.Pos

object Day08 extends Aoc2024("input_08.txt"):
  val grid: Grid[Char] = input.toGrid()

  val antennas = grid.toPosTuples.groupMap(_._2)(_._1).view.filterKeys(_ != '.').toMap

  val antinodes = antennas.values.flatMap: positions =>
    val pairs = positions.combinations(2)
    pairs
      .flatMap:
        case Array(a, b) =>
          Array(
            a + a - b,
            b + b - a
          )
      .filter(_.isInsideGrid(grid))

  println(antinodes.toSet.size) // 413

  def antinodes(start: Pos, delta: Pos): Set[Pos] =
    LazyList
      .unfold(start) { pos =>
        val newPos = pos + delta
        if newPos.isInsideGrid(grid) then Some(newPos -> newPos)
        else None
      }
      .toSet

  val resonantAntinodes = antennas.values
    .filter(_.length >= 3)
    .flatMap: positions =>
      val pairs = positions.combinations(2)
      pairs
        .flatMap:
          case Array(a, b) =>
            val delta = b - a
            val nodes = antinodes(a, delta) ++ antinodes(b, -delta)
            nodes

  println((resonantAntinodes.toSet ++ antennas.values.flatten.toSet).size) // 1417
