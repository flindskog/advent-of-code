package aoc.y2023

import aoc.data.{Pos, WeightedDigraph, WeightedEdge}

import scala.annotation.tailrec

object Day23 extends Aoc2023("input_23.txt"):
  val grid = input.toGrid[Char]()

  val pathCells = grid.indices.flatMap { row =>
    grid(row).indices.flatMap { col =>
      val cell = grid(row)(col)
      if (cell != '#')
        Some(Pos(row, col) -> cell)
      else
        None
    }
  }.toMap

  def onEdge(pos: Pos, grid: Array[Array[Char]]): Boolean =
    pos.row == 0 || pos.row == grid.length - 1 || pos.col == 0 || pos.col == grid.head.length - 1

  def isValidNeighbor(from: Pos, neighbor: Pos): Boolean =
    pathCells(neighbor) == '.' || {
      val direction = neighbor - from
      direction match {
        case Pos(0, 1)  => pathCells(neighbor) == '>'
        case Pos(0, -1) => pathCells(neighbor) == '<'
        case Pos(1, 0)  => pathCells(neighbor) == 'v'
        case Pos(-1, 0) => pathCells(neighbor) == '^'
        case _          => sys.error(s"Invalid direction $direction")
      }
    }

  val verticesWithNeighbors: Map[Pos, Set[Pos]] = pathCells.keys.flatMap { pos =>
    val neighbours = pos.adjacentNeighbors intersect pathCells.keySet
    val valid      = neighbours.filter(isValidNeighbor(pos, _))
    if neighbours.size > 2 || onEdge(pos, grid) then Some(pos -> valid)
    else None
  }.toMap

  println(verticesWithNeighbors)

  @tailrec
  def walk(
      vertices: Map[Pos, Set[Pos]],
      isValid: (Pos, Pos) => Boolean
  )(start: Pos, pos: Pos, visited: Set[Pos]): Option[WeightedEdge[Pos, Int]] = {
    val isEndpoint = vertices.keySet.contains(pos)
    if isEndpoint then Some(WeightedEdge(start, pos, visited.size))
    else {
      val neighbors = pos.adjacentNeighbors intersect pathCells.keySet diff visited
      if neighbors.isEmpty || !isValid(pos, neighbors.head) then None
      else walk(vertices, isValid)(start, neighbors.head, visited + pos)
    }
  }

  val edges = verticesWithNeighbors.flatMap { case (pos, neighbors) =>
    neighbors.flatMap { neighbor =>
      walk(verticesWithNeighbors, isValidNeighbor)(pos, neighbor, Set(pos))
    }
  }

  val graph = edges.toSeq.foldLeft[WeightedDigraph[Pos, Int]](WeightedDigraph()) { (acc, edge) =>
    acc.addEdge(edge.copy(weight = -edge.weight))
  }

  val start = Pos(0, 1)
  val end   = Pos(140, 139)

  val res = -graph.shortestDistance(start, end).get
  println(res) // 2206

  // Part 2
  val verticesWithNeighbors2 = pathCells.keys.flatMap { pos =>
    val neighbours = pos.adjacentNeighbors intersect pathCells.keySet
    if neighbours.size > 2 || onEdge(pos, grid) then Some(pos -> neighbours)
    else None
  }.toMap

  val isValid = (_: Pos, to: Pos) => Set('.', '<', '>', '^', 'v').contains(pathCells(to))

  val edges2 = verticesWithNeighbors2.flatMap { case (pos, neighbors) =>
    neighbors.flatMap { neighbor =>
      walk(verticesWithNeighbors2, isValid)(pos, neighbor, Set(pos))
    }
  }

  val graph2 = edges2.toSeq.foldLeft[WeightedDigraph[Pos, Int]](WeightedDigraph()) { (acc, edge) =>
    acc.addEdge(edge.copy(weight = edge.weight))
  }

  val res2 = graph2.longestSimplePath(start, end).get
  println(res2) // 6498
