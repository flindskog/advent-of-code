package aoc.utils

import scala.collection.mutable

object Dijkstra {
  def searchAllPaths[T](
      start: T,
      isTarget: T => Boolean,
      neighbors: T => Set[(T, Int)]
  ): (Int, List[List[T]]) = {
    val dist  = mutable.Map[T, Int]().withDefaultValue(Int.MaxValue)  // Distance map
    val paths = mutable.Map[T, List[List[T]]]().withDefaultValue(Nil) // Paths map
    val pq    = mutable.PriorityQueue[(T, Int)]()(Ordering.by(-_._2)) // Min-heap priority queue

    dist(start) = 0
    paths(start) = List(List(start))
    pq.enqueue((start, 0))

    while (pq.nonEmpty) {
      val (current, currentDist) = pq.dequeue()

      // Skip if we've already processed a shorter distance for this node
      if (currentDist <= dist(current)) {
        for ((neighbor, weight) <- neighbors(current)) {
          val newDist = dist(current) + weight

          if (newDist < dist(neighbor)) {
            // Found a new shortest distance
            dist(neighbor) = newDist
            paths(neighbor) = paths(current).map(path => path :+ neighbor)
            pq.enqueue((neighbor, newDist))
          } else if (newDist == dist(neighbor)) {
            // Found an additional shortest path
            paths(neighbor) ++= paths(current).map(path => path :+ neighbor)
          }
        }
      }
    }

    // Return the shortest distance and all its paths to the target
    val (targetTile, distance) = dist.filterKeys(isTarget).minBy(_._2)
    val targetPaths = paths(targetTile)

    (distance, targetPaths)
  }
}
