package aoc.y2022

object Day03 extends Aoc2022("input_03.txt"):

  def score(letter: Char): Int =
    if letter.isUpper then letter - 'A' + 27 else letter - 'a' + 1

  val result = input.map: line =>
    val (first, second) = line.splitAt(line.length / 2)
    val common          = first.toSet.intersect(second.toSet)
    common
      .map(score)
      .sum

  println(result.sum)

  val result2 = input
    .grouped(3)
    .map: ll =>
      val common = ll.toList match
        case List(a, b, c) =>
          a.toSet intersect b.toSet intersect c.toSet
        case _ => sys.error("invalid input")
      common
        .map(score)
        .sum

  println(result2.sum)
