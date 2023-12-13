package aoc.syntax

trait OptionSyntax {
  extension [T](o: T) def some: Option[T] = Some(o)
}
