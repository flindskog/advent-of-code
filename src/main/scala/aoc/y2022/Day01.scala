package aoc.y2022

object Day01 extends Aoc2022("input_01.txt"):
  val data = input.splitByEmptyLine.map(_.map(_.toLong).sum)

  val result = data.max
  println(result) // 69177

  val result2 = data.sorted.reverse.take(3).sum
  println(result2) // 207456
