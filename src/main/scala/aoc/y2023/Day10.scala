package aoc.y2023

import scala.annotation.tailrec
import scala.util.Try

private case class Position(row: Int, col: Int):
  def neighbors: Set[Position] =
    Set(Position(row + 1, col), Position(row - 1, col), Position(row, col + 1), Position(row, col - 1))

private case class Vertex(char: Char, position: Position, edges: Set[Position])

object Day10 extends Aoc2023("input_10.txt"):
  def breadthFirstTraversal(start: Vertex, graph: Map[Position, Vertex]): List[(Vertex, Int)] =
    @tailrec
    def loop(queue: List[(Vertex, Int)], visited: Map[Vertex, Int]): Map[Vertex, Int] =
      queue match
        case Nil => visited
        case (vertex, distance) :: tail =>
          val newVisited = visited + (vertex -> distance)
          val newQueue =
            tail ++ vertex.edges.map(graph).filter(vtx => !newVisited.contains(vtx)).map(_ -> (distance + 1))
          loop(newQueue, newVisited)

    loop(List(start -> 0), Map.empty).toList

  val grid = input
    .map(_.toArray)
    .toArray

  val graph: Map[Position, Vertex] = grid.zipWithIndex
    .flatMap: (rowArray, row) =>
      rowArray.zipWithIndex.flatMap: (char, col) =>
        val position = Position(row, col)
        char match
          case '|' => Vertex(char, position, Set(Position(row + 1, col), Position(row - 1, col))).some
          case '-' => Vertex(char, position, Set(Position(row, col + 1), Position(row, col - 1))).some
          case 'L' => Vertex(char, position, Set(Position(row - 1, col), Position(row, col + 1))).some
          case 'J' => Vertex(char, position, Set(Position(row - 1, col), Position(row, col - 1))).some
          case '7' => Vertex(char, position, Set(Position(row + 1, col), Position(row, col - 1))).some
          case 'F' => Vertex(char, position, Set(Position(row + 1, col), Position(row, col + 1))).some
          case 'S' =>
            val west =
              Try(grid(row)(col - 1)).toOption.filter(Set('-', 'L', 'F').contains).map(_ => Position(row, col - 1))
            val east =
              Try(grid(row)(col + 1)).toOption.filter(Set('-', 'J', '7').contains).map(_ => Position(row, col + 1))
            val north =
              Try(grid(row - 1)(col)).toOption.filter(Set('|', 'F', '7').contains).map(_ => Position(row - 1, col))
            val south =
              Try(grid(row + 1)(col)).toOption.filter(Set('|', 'L', 'J').contains).map(_ => Position(row + 1, col))

            val char = (west, east, north, south) match
              case (Some(_), Some(_), None, None) => '-'
              case (Some(_), None, Some(_), None) => 'J'
              case (Some(_), None, None, Some(_)) => '7'
              case (None, Some(_), Some(_), None) => 'L'
              case (None, Some(_), None, Some(_)) => 'F'
              case (None, None, Some(_), Some(_)) => '|'
              case _                              => sys.error(s"Unknown start char $west $east $north $south")

            Vertex(char, position, Set(west, east, north, south).flatten).some
          case '.' => None
          case _   => sys.error(s"Unknown char $char")
    .map(vertex => vertex.position -> vertex)
    .toMap

  val start: Position = grid.zipWithIndex
    .flatMap: (rowArray, row) =>
      rowArray.zipWithIndex.flatMap: (char, col) =>
        if char == 'S' then Position(row, col).some else None
    .headOption
    .getOrElse(sys.error("No start found"))

  val result = breadthFirstTraversal(graph(start), graph)
  println(result.maxBy(_._2)._2) // 6754

  // Part 2
  val loop = result.map { (vertex, _) =>
    vertex.position -> vertex
  }.toMap

  val empty = grid.indices flatMap { row =>
    grid.head.indices flatMap { col =>
      val pos = Position(row, col)
      loop.get(pos) match
        case Some(_) => None
        case None    => Some(pos)
    }
  }

  // Finds the number of bloccking bordes from the position to the top
  def rayTraceUp(position: Position): Int = (position.row - 1 to 0 by -1).map { (row: Int) =>
    loop.get(position.copy(row = row)).map(_.char) match
      case Some('-') | Some('J') | Some('7') => 1
      case _                                 => 0
  }.sum

  // A position is enclosed if the number of ray traced blocking borders is odd
  val enclosed = empty.map(v => v -> rayTraceUp(v)).filter((_, count) => count % 2 == 1)
  println(enclosed.size) // 567
