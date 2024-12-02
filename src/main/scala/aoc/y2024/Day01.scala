package aoc.y2024

object Day01 extends Aoc2024("input_01.txt"):

  val (left, right) = input
    .map: str =>
      val numbers = str.split(" +").map(_.toInt)
      (numbers(0), numbers(1))
    .unzip

  val lSorted = left.sorted
  val rSorted = right.sorted

  val result1 = lSorted
    .zip(rSorted)
    .map: (l, r) =>
      math.abs(l - r)
    .sum

  println(result1) // 2031679

  val occurences = right.groupBy(identity).view.mapValues(_.size).toMap

  val result2 = lSorted
    .map: l =>
      l * occurences.getOrElse(l, 0)
    .sum

  println(result2) // 19678534
