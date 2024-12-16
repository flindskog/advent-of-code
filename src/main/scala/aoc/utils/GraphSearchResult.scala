package aoc.utils

sealed trait GraphSearchResult[T]

object GraphSearchResult:
  case class Found[T](target: T, distance: Int, path: Seq[T]) extends GraphSearchResult[T]
  case class NotFound[T]()                                    extends GraphSearchResult[T]
