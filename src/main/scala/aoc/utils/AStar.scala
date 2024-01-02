package aoc.utils

import aoc.utils.graph.SearchResult

import scala.collection.mutable

type Heuristics[T] = T => Int

object AStar {
  private case class State[T](t: T, distance: Int, heuristicValue: Int)

  def search[T](
      start: T,
      isTarget: T => Boolean,
      neighbors: T => Set[(T, Int)],
      heuristics: Heuristics[T]
  ): SearchResult[T] = {
    // Mutable data structures are used for performance reasons
    val frontier         = mutable.PriorityQueue.empty[State[T]](Ordering.by(-_.heuristicValue))
    val visitedDistances = mutable.Map.empty[T, Int]

    frontier.enqueue(State(start, 0, 0))

    while (frontier.nonEmpty) {
      val State(node, dist, _) = frontier.dequeue()
      if (!visitedDistances.contains(node)) {
        visitedDistances += (node -> dist)
        if (isTarget(node)) {
          return SearchResult.Found(node, dist, visitedDistances.toMap)
        }
        neighbors(node).foreach((n, d) => frontier.enqueue(State(n, dist + d, dist + d + heuristics(n))))
      }
    }
    SearchResult.NotFound(visitedDistances.toMap)
  }
}
