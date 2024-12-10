package aoc.y2024

import aoc.data.{Direction, Pos}

import scala.collection.mutable

object Day06 extends Aoc2024("input_06.txt"):
  val grid = input.toGrid(identity)

  enum Type:
    case Obstacle, Empty, Start

  val data = grid.indices
    .flatMap: row =>
      grid(row).indices.map: col =>
        grid(row)(col) match
          case '#' => Type.Obstacle -> Pos(row, col)
          case '.' => Type.Empty    -> Pos(row, col)
          case '^' => Type.Start    -> Pos(row, col)
    .groupMap(_._1)(_._2)

  val obstacles = data(Type.Obstacle).toSet
  val start     = data(Type.Start).head

  def walk(obstacles: Set[Pos], pos: Pos, direction: Direction): LazyList[(Pos, Direction)] =
    LazyList.unfold((pos, direction)) { (pos, direction) =>
      if pos.isInsideGrid(grid) then
        val nextPos = pos.move(direction)
        if obstacles.contains(nextPos)
        then Some((pos, direction) -> (pos     -> direction.turnRight))
        else Some((pos, direction) -> (nextPos -> direction))
      else None
    }

  val theWalk = walk(obstacles, start, Direction.Up)

  val result = theWalk.map(_._1).toSet.size
  println(result) // 5080

  val infinityObstacles = theWalk.foldLeft(Set.empty[Pos]) { case (acc, (pos, direction)) =>
    val obstacle = pos.move(direction)
    if !obstacles.contains(obstacle) && obstacle.isInsideGrid(grid) then
      val visited = mutable.Set.empty[(Pos, Direction)]
      val maybeInfWalk =
        walk(obstacles + obstacle, start, Direction.Up).dropWhile(visited.add)

      if maybeInfWalk.nonEmpty then acc + obstacle else acc
    else acc
  }

  val result2 = infinityObstacles - start

  println(result2.size) // 1919

  /*
  Thank you @ikr for the insights!
   */
