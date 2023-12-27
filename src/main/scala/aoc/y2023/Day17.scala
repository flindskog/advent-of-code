package aoc.y2023

import aoc.data.*
import aoc.utils.{Dijkstra, GridUtils}

object Day17 extends Aoc2023("input_17.txt"):

  val grid = input.toGrid(_.toString.toInt)

  case class Key(pos: Pos, direction: Direction, stepsTakenInDirection: Int)

  val start = Key(Pos(0, 0), Direction.Right, 0)

  def isTarget(minSteps: Int)(key: Key): Boolean =
    key.pos == Pos(grid.length - 1, grid.head.length - 1) && key.stepsTakenInDirection >= minSteps

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
    Dijkstra.search(start, isTarget(minSteps), neighbors(minSteps, maxSteps))

  val res1 = solve(1, 3)
  println(res1.target.get._2) // 767

  val res2 = solve(4, 10)
  println(res2.target.get._2) // 904
