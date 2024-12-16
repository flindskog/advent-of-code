package aoc.utils

import scala.collection.mutable

type Heuristics[T] = T => Int

object AStar {
  private case class State[T](t: T, distance: Int, heuristicValue: Int)

  def search[T](
      start: T,
      isTarget: T => Boolean,
      neighbors: T => Set[(T, Int)],
      heuristics: Heuristics[T]
  ): GraphSearchResult[T] = {
    // Mutable data structures are used for performance reasons
    val frontier         = mutable.PriorityQueue.empty[State[T]](Ordering.by(-_.heuristicValue))
    val visitedDistances = mutable.Map.empty[T, Int]
    val predecessors     = mutable.Map.empty[T, T]

    frontier.enqueue(State(start, 0, 0))

    while (frontier.nonEmpty) {
      val State(node, distance, _) = frontier.dequeue()
      if (!visitedDistances.contains(node)) {
        visitedDistances += (node -> distance)
        if (isTarget(node)) {
          val path = reconstructPath(predecessors, start, node)
          return GraphSearchResult.Found(node, distance, path)
        }
        neighbors(node).foreach { case (n, d) =>
          val newDistance    = distance + d
          val heuristicValue = newDistance + heuristics(n)
          if (!visitedDistances.contains(n) || newDistance < visitedDistances(n)) {
            frontier.enqueue(State(n, newDistance, heuristicValue))
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
