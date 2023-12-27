package aoc.utils

import scala.collection.mutable

object Dijkstra {
  case class Result[T](distances: Map[T, Int], target: Option[(T, Int)])

  def search[T](start: T, isTarget: T => Boolean, neighbors: T => Set[(T, Int)]): Result[T] = {
    // Mutable data structures are used for performance reasons
    val frontier         = mutable.PriorityQueue.empty[(T, Int)](Ordering.by((_, distance) => -distance))
    val visitedDistances = mutable.Map.empty[T, Int]

    frontier.enqueue((start, 0))

    while (frontier.nonEmpty) {
      val (node, dist) = frontier.dequeue()
      if (!visitedDistances.contains(node)) {
        visitedDistances += (node -> dist)
        if (isTarget(node)) {
          return Result(visitedDistances.toMap, Some(node -> dist))
        }
        neighbors(node).foreach { case (n, d) => frontier.enqueue((n, dist + d)) }
      }
    }
    Result(visitedDistances.toMap, None)
  }
}
