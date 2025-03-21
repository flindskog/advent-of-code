package aoc.y2024

import scala.collection.mutable

object Day19 extends Aoc2024("input_19.txt"):
  val (towelString, designs) = input.splitByEmptyLine match
    case LazyList(t #:: LazyList(), d) => t -> d
    case _                             => sys.error("Invalid input")

  val towels = towelString.split(", ")

  val cache = mutable.Map[String, Long]()
  def matches(design: String): Long =
    cache.getOrElseUpdate(
      design,
      towels
        .filter(design.startsWith)
        .map { t =>
          val rest = design.drop(t.length)
          if rest.isEmpty then 1
          else matches(rest)
        }
        .sum
    )

  val m = designs.map(matches)
  println(m.count(_ > 0)) // 322
  println(m.sum)          // 715514563508258
