package aoc.y2024

import aoc.data.Pos
import aoc.utils.{GraphSearch, GraphSearchResult}

import scala.annotation.tailrec

object Day18 extends Aoc2024("input_18.txt"):
  val fallenBytes = input.map: s =>
    val Array(x, y) = s.split(',').map(_.toInt)
    Pos(x, y)

  val xBoundary = 70
  val yBoundary = 70
  val startPos  = Pos(0, 0)
  val endPos    = Pos(xBoundary, yBoundary)
  val noFallen  = 1024
  val fallen    = fallenBytes.take(noFallen).toSet

  def neighbors(fallen: Set[Pos])(pos: Pos): Set[Pos] =
    pos.adjacentNeighbors
      .filter(!fallen.contains(_))
      .filter(p => p.row >= 0 && p.row <= xBoundary && p.col >= 0 && p.col <= yBoundary)

  GraphSearch.bfs(startPos, _ == endPos, neighbors(fallen)) match
    case GraphSearchResult.Found(_, distance, _) => println(distance) // 436
    case GraphSearchResult.NotFound()            => println("Error, not found")

  def findFirstUnreachable: Option[Pos] =
    @tailrec
    def loop(noFallen: Int): Option[Pos] =
      val fallen = fallenBytes.take(noFallen).toSet
      GraphSearch.bfs(startPos, _ == endPos, neighbors(fallen)) match
        case GraphSearchResult.Found(_, _, _) => loop(noFallen + 1)
        case GraphSearchResult.NotFound()     => Some(fallenBytes(noFallen - 1))

    loop(noFallen)

  findFirstUnreachable match
    case Some(value) => println(value) // 61,50
    case None        => println("Error, not found")
