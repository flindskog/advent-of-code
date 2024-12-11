package aoc.y2024

object Day11 extends Aoc2024("input_11.txt"):
  val stones = input.head.split(" ").map(_.toLong).toSeq

  def transform(stone: Long): Seq[Long] =
    if stone == 0 then Seq(1)
    else if stone.toString.length % 2 == 0 then
      stone.toString.splitAt(stone.toString.length / 2) match
        case (left, right) =>
          Seq(left.toLong, right.toLong)
    else Seq(stone * 2024)

  def blink(times: Int): Seq[Long] =
    (1 to times)
      .foldLeft(stones) { case (acc, i) =>
        acc.flatMap(transform)
      }

  val result = blink(25).size
  println(result) // 194782
