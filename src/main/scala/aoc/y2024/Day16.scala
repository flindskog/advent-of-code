package aoc.y2024

import aoc.data.{Direction, Pos}
import aoc.utils.{Dijkstra, GraphSearch, GraphSearchResult, SeqGraphSearchResult}

object Day16 extends Aoc2024("input_16.txt"):
  case class Tile(position: Pos, direction: Direction) {
    def turnLeft: Tile  = Tile(position, direction.turnLeft)
    def turnRight: Tile = Tile(position, direction.turnRight)
    def move: Tile      = Tile(position.move(direction), direction)
  }

  val data = input.toGrid(identity).toPosTuples.filter(_._2 != '#')
  val maze = data.map(_._1).toSet

  val startPosition = data.find(_._2 == 'S') match
    case Some((pos, _)) => pos
    case None           => throw new Exception("No start found")
  val start = Tile(startPosition, Direction.Right)

  val endPosition = data.find(_._2 == 'E') match
    case Some((pos, _)) => pos
    case None           => throw new Exception("No end found")
  def isEnd(tile: Tile): Boolean = tile.position == endPosition

  def neighbors(tile: Tile): Set[(Tile, Int)] =
    Set(
      tile.turnLeft  -> 1000,
      tile.turnRight -> 1000,
      tile.move      -> 1
    ).filter((t, _) => maze.contains(t.position))

  val result = GraphSearch.dijkstra(start, isEnd, neighbors)

  result match {
    case GraphSearchResult.Found(_, distance, _) => println(distance) // 94444
    case GraphSearchResult.NotFound()            => println("Error, not found")
  }

  val (_, result2) = Dijkstra.searchAllPaths(start, isEnd, neighbors)
  println(result2.flatten.toSet.map(_.position).size) // 502