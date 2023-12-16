package aoc.y2023

import aoc.data.Graph

object Day16 extends Aoc2023("input_16.txt"):
  enum Direction:
    case Up, Right, Down, Left

  case class Position(row: Int, col: Int) {
    def move(direction: Direction): Position = direction match {
      case Direction.Up    => Position(row - 1, col)
      case Direction.Down  => Position(row + 1, col)
      case Direction.Left  => Position(row, col - 1)
      case Direction.Right => Position(row, col + 1)
    }
  }

  case class Beam(position: Position, direction: Direction)

  def isInsideGrid(position: Position): Boolean =
    position.row >= 0 && position.row < grid.length && position.col >= 0 && position.col < grid(position.row).length

  val grid        = input.map(_.toCharArray).toArray
  val gridIndices = grid.indices.flatMap(row => grid(row).indices.map(col => (row, col)))
  val graph = gridIndices.foldLeft(Graph.empty[Beam]) { case (graph, (row, col)) =>
    val position   = Position(row, col)
    val directions = List(Direction.Up, Direction.Down, Direction.Left, Direction.Right)
    val keys       = directions.map(Beam(position, _))
    val square     = grid(row)(col)
    val graphData = keys.map { key =>
      val vertexDirections: Set[Direction] = square match {
        case '.' =>
          Set(key.direction)
        case '/' =>
          Set(key.direction match {
            case Direction.Up    => Direction.Right
            case Direction.Down  => Direction.Left
            case Direction.Left  => Direction.Down
            case Direction.Right => Direction.Up
          })
        case '\\' =>
          Set(key.direction match {
            case Direction.Up    => Direction.Left
            case Direction.Down  => Direction.Right
            case Direction.Left  => Direction.Up
            case Direction.Right => Direction.Down
          })
        case '|' =>
          key.direction match {
            case Direction.Up | Direction.Down => Set(key.direction)
            case Direction.Left | Direction.Right =>
              Set(Direction.Up, Direction.Down)
          }
        case '-' =>
          key.direction match {
            case Direction.Left | Direction.Right => Set(key.direction)
            case Direction.Up | Direction.Down =>
              Set(Direction.Left, Direction.Right)
          }
      }
      key -> vertexDirections.map(d => Beam(position.move(d), d)).filter(k => isInsideGrid(k.position))
    }.toMap
    graphData.foldLeft(graph) { case (graph, (key, edges)) =>
      graph
        .addVertex(key)
        .addEdges(key, edges)
    }
  }

  val start = Beam(Position(0, 0), Direction.Right)

  val startBeams =
    grid.indices.flatMap { row =>
      List(
        Beam(Position(row, 0), Direction.Right),
        Beam(Position(row, grid(row).length - 1), Direction.Left)
      )
    }.toList ::: grid(0).indices.flatMap { col =>
      List(
        Beam(Position(0, col), Direction.Down),
        Beam(Position(grid.length - 1, col), Direction.Up)
      )
    }.toList

  println(graph.depthFirstTraversal(start).map(_.position).toSet.size)                 // 7632
  println(startBeams.map(graph.depthFirstTraversal(_).map(_.position).toSet.size).max) // 8023
