package aoc.y2023

import aoc.data.*
import aoc.utils.{GraphSearch, GraphSearchResult, GridUtils}

object Day17 extends Aoc2023("input_17.txt"):

  val grid = input.toGrid(_.toString.toInt)

  case class Key(pos: Pos, direction: Direction, stepsTakenInDirection: Int)

  val start = Key(Pos(0, 0), Direction.Right, 0)

  val endPos = Pos(grid.length - 1, grid.head.length - 1)

  def isTarget(minSteps: Int)(key: Key): Boolean =
    key.pos == endPos && key.stepsTakenInDirection >= minSteps

  def neighbors(minSteps: Int, maxSteps: Int)(key: Key): Set[(Key, Int)] = {
    val nextAhead =
      if (key.stepsTakenInDirection < maxSteps)
        Set(Key(key.pos.move(key.direction), key.direction, key.stepsTakenInDirection + 1))
      else Set()
    val nextTurn =
      if (
        key.stepsTakenInDirection >= minSteps ||
        key == start // Doh, start node can also go down
      )
        Set(
          Key(key.pos.move(key.direction.turnLeft), key.direction.turnLeft, 1),
          Key(key.pos.move(key.direction.turnRight), key.direction.turnRight, 1)
        )
      else Set()
    (nextAhead ++ nextTurn)
      .filter(key => GridUtils.isInsideGrid(grid, key.pos))
      .map(k => k -> grid(k.pos.row)(k.pos.col))
  }

  def solve(minSteps: Int, maxSteps: Int) =
    GraphSearch.aStar(start, isTarget(minSteps), neighbors(minSteps, maxSteps), _.pos.manhattanDistance(endPos))

  solve(1, 3) match {
    case GraphSearchResult.Found(_, distance, _) => println(distance) // 767
    case GraphSearchResult.NotFound()            => println(s"Error, not found")
  }

  solve(4, 10) match {
    case GraphSearchResult.Found(_, distance, _) => println(distance) // 904
    case GraphSearchResult.NotFound()            => println(s"Error, not found")
  }
