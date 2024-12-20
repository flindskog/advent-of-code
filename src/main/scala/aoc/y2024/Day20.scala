package aoc.y2024

import aoc.data.Pos
import aoc.utils.GraphSearch
import aoc.utils.GraphSearchResult.{Found, NotFound}

object Day20 extends Aoc2024("input_20.txt"):
  val data = input.toGrid(identity).toPosTuples.filter(_._2 != '#')
  val maze = data.map(_._1).toSet

  val start = data.find(_._2 == 'S') match
    case Some((pos, _)) => pos
    case None           => throw new Exception("No start found")

  val end = data.find(_._2 == 'E') match
    case Some((pos, _)) => pos
    case None           => throw new Exception("No end found")
  def isEnd(pos: Pos): Boolean = pos == end

  def neighbors(pos: Pos): Set[Pos] =
    pos.adjacentNeighbors.intersect(maze)

  val pathway = GraphSearch.bfs(start, isEnd, neighbors) match
    case Found(_, _, path) => path.zipWithIndex
    case NotFound()        => throw new Exception("No path found")

  val pathMap = pathway.toMap

  def findCheats(minSave: Int, maxDist: Int): Int =
    val (_, count) = pathway.foldLeft((pathMap, 0)) { case ((remainingPathMap, accCount), (currentPos, currentSteps)) =>
      val cheats = remainingPathMap.filter: (pos, steps) =>
        val dst   = pos.manhattanDistance(currentPos)
        val saved = steps - currentSteps - dst
        saved >= minSave && dst <= maxDist

      (remainingPathMap - currentPos, accCount + cheats.size)
    }

    count

  println(findCheats(100, 2))  // 1463
  println(findCheats(100, 20)) // 985332
