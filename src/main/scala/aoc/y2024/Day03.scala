package aoc.y2024

object Day03 extends Aoc2024("input_03.txt"):

  val lineRegex = """mul\([0-9]{1,3},[0-9]{1,3}\)""".r
  val mulRegex  = """mul\(([0-9]{1,3}),([0-9]{1,3})\)""".r

  val result1 = input
    .flatMap(lineRegex.findAllIn)
    .map: s =>
      val mulRegex(a, b) = s
      a.toLong * b.toLong
    .sum

  println(result1) // 164730528

  
