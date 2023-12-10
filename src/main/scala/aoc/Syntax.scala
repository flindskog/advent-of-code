package aoc

trait Syntax {
  extension [T](o: T) def some: Option[T] = Some(o)
}
