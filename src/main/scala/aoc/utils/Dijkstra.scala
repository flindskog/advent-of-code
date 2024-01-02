package aoc.utils

import aoc.utils.graph.SearchResult

import scala.collection.mutable

object Dijkstra {
  def search[T](start: T, isTarget: T => Boolean, neighbors: T => Set[(T, Int)]): SearchResult[T] = {
    // Mutable data structures are used for performance reasons
    val frontier         = mutable.PriorityQueue.empty[(T, Int)](Ordering.by((_, distance) => -distance))
    val visitedDistances = mutable.Map.empty[T, Int]

    frontier.enqueue((start, 0))

    while (frontier.nonEmpty) {
      val (node, dist) = frontier.dequeue()
      if (!visitedDistances.contains(node)) {
        visitedDistances += (node -> dist)
        if (isTarget(node)) {
          return SearchResult.Found(node, dist, visitedDistances.toMap)
        }
        neighbors(node).foreach((n, d) => frontier.enqueue((n, dist + d)))
      }
    }
    SearchResult.NotFound(visitedDistances.toMap)
  }
}
