package aoc.utils

package graph:
  sealed trait SearchResult[T]:
    def distances: Map[T, Int]

  object SearchResult:
    case class Found[T](target: T, distance: Int, distances: Map[T, Int]) extends SearchResult[T]
    case class NotFound[T](distances: Map[T, Int])                        extends SearchResult[T]
