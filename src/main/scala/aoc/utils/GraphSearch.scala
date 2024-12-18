package aoc.utils

import scala.collection.mutable

object GraphSearch {
  def dijkstra[T](start: T, isTarget: T => Boolean, neighbors: T => Set[(T, Int)]): GraphSearchResult[T] = {
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

  type Heuristics[T] = T => Int
  private case class State[T](t: T, distance: Int, heuristicValue: Int)

  def aStar[T](
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

  def bfs[T](start: T, isTarget: T => Boolean, neighbors: T => Set[T]): GraphSearchResult[T] = {
    val queue        = mutable.Queue.empty[T]
    val visited      = mutable.Set.empty[T]
    val predecessors = mutable.Map.empty[T, T]

    queue.enqueue(start)

    while (queue.nonEmpty) {
      val node = queue.dequeue()
      if (!visited.contains(node)) {
        visited += node
        if (isTarget(node)) {
          val path = reconstructPath(predecessors, start, node)
          return GraphSearchResult.Found(node, path.length - 1, path)
        }
        neighbors(node).foreach { n =>
          if (!visited.contains(n)) {
            queue.enqueue(n)
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
