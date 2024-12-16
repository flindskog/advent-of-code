package aoc.utils

import scala.collection.mutable

object Dijkstra {
  def search[T](start: T, isTarget: T => Boolean, neighbors: T => Set[(T, Int)]): GraphSearchResult[T] = {
    val frontier     = mutable.PriorityQueue.empty[(T, Int)](Ordering.by((_, distance) => -distance))
    val visited      = mutable.Map.empty[T, Int]
    val predecessors = mutable.Map.empty[T, T]

    frontier.enqueue((start, 0))

    while (frontier.nonEmpty) {
      val (node, dist) = frontier.dequeue()
      if (!visited.contains(node)) {
        visited += (node -> dist)
        if (isTarget(node)) {
          val path = reconstructPath(predecessors, start, node)
          return GraphSearchResult.Found(node, dist, path)
        }
        neighbors(node).foreach { case (n, d) =>
          if (!visited.contains(n) || dist + d < visited(n)) {
            frontier.enqueue((n, dist + d))
            predecessors += (n -> node)
          }
        }
      }
    }
    GraphSearchResult.NotFound()
  }

  private def reconstructPath[T](predecessors: mutable.Map[T, T], start: T, target: T): Seq[T] = {
    var path    = List(target)
    var current = target
    while (current != start) {
      current = predecessors(current)
      path = current :: path
    }
    path
  }
}
