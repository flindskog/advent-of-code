package aoc.data

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable

case class WeightedEdge[T, W](from: T, to: T, weight: W)

case class WeightedDigraph[T, W](vertices: Set[T] = Set(), edges: Set[WeightedEdge[T, W]] = Set())(using
    numeric: Numeric[W]
) {
  val edgesByKey = edges.groupBy(_.from)

  def addVertex(vertex: T): WeightedDigraph[T, W] =
    WeightedDigraph(vertices + vertex, edges)
    
  def addEdge(edge: WeightedEdge[T, W]): WeightedDigraph[T, W] =
    WeightedDigraph(vertices + edge.from + edge.to, edges + edge)

  /**
   * Returns the shortest path from `from` to `to` in the graph.
   * It handles negative as well as positive weights.
   */
  def shortestDistance(from: T, to: T): Option[W] = {
    val distances    = vertices.map(_ -> numeric.zero).toMap + (from -> numeric.zero)
    val predecessors = vertices.map(_ -> Option.empty[T]).toMap
    val edgesList    = edges.toList

    @tailrec
    def relaxEdges(
        edges: List[WeightedEdge[T, W]],
        distances: Map[T, W],
        predecessors: Map[T, Option[T]]
    ): (Map[T, W], Map[T, Option[T]]) =
      edges match {
        case Nil => (distances, predecessors)
        case WeightedEdge(from, to, weight) :: tail =>
          val newDistance = numeric.plus(distances(from), weight)
          if (numeric.lt(newDistance, distances(to))) {
            relaxEdges(tail, distances + (to -> newDistance), predecessors + (to -> Some(from)))
          } else {
            relaxEdges(tail, distances, predecessors)
          }
      }

    @tailrec
    def relaxEdgesNTimes(
        edges: List[WeightedEdge[T, W]],
        distances: Map[T, W],
        predecessors: Map[T, Option[T]],
        n: Int
    ): (Map[T, W], Map[T, Option[T]]) =
      if (n == 0) {
        (distances, predecessors)
      } else {
        val (newDistances, newPredecessors) = relaxEdges(edges, distances, predecessors)
        relaxEdgesNTimes(edges, newDistances, newPredecessors, n - 1)
      }

    val (newDistances, newPredecessors) = relaxEdgesNTimes(edgesList, distances, predecessors, vertices.size - 1)

    val (finalDistances, finalPredecessors) = relaxEdges(edgesList, newDistances, newPredecessors)

    if (finalDistances(to) == numeric.zero) {
      None
    } else {
      Some(finalDistances(to))
    }
  }

  def longestSimplePath(from: T, to: T): Option[W] = {
    def dfs(current: T, visited: Set[T]): Option[W] =
      if (current == to) Some(numeric.zero)
      else {
        val outgoingEdges = edgesByKey.getOrElse(current, Set())
        val possiblePaths = for {
          edge <- outgoingEdges
          if !visited(edge.to)
          nextPath <- dfs(edge.to, visited + edge.to)
        } yield numeric.plus(edge.weight, nextPath)

        possiblePaths.maxOption
      }

    dfs(from, Set(from))
  }

  def longestSimplePathOptimized(from: T, to: T): Option[W] = {
    var memo: Map[(T, Set[T]), Option[W]] = Map()

    def dfs(current: T, visited: Set[T]): Option[W] =
      if (current == to) Some(numeric.zero)
      else {
        val subproblem = (current, visited)
        if (memo.contains(subproblem)) {
          memo(subproblem)
        } else {
          val outgoingEdges = edgesByKey.getOrElse(current, Set())
          val possiblePaths = for {
            edge <- outgoingEdges
            if !visited(edge.to)
            nextPath <- dfs(edge.to, visited + edge.to)
          } yield numeric.plus(edge.weight, nextPath)

          val result = possiblePaths.maxOption
          memo += (subproblem -> result)
          result
        }
      }

    dfs(from, Set(from))
  }
}

object WeightedDigraph {
  def apply[T, W]()(using numeric: Numeric[W]): WeightedDigraph[T, W] = WeightedDigraph(Set(), Set())
}
