package aoc.y2024

import scala.annotation.tailrec

object Day11 extends Aoc2024("input_11.txt"):
  val stones =
    input.head.split(" ").map(_.toLong).toSeq.foldLeft(Map.empty[Long, Long].withDefault(_ => 0L)) { (acc, stone) =>
      acc.updated(stone, acc(stone) + 1L)
    }

  def transform(stone: Long): Seq[Long] =
    if stone == 0 then Seq(1)
    else if stone.toString.length % 2 == 0 then
      stone.toString.splitAt(stone.toString.length / 2) match
        case (left, right) =>
          Seq(left.toLong, right.toLong)
    else Seq(stone * 2024)

  @tailrec
  def blink(times: Int, stones: Map[Long, Long]): Map[Long, Long] =
    if times == 0 then stones
    else {
      val transformed = stones
        .map: (stone, count) =>
          transform(stone).map(_ -> count)
        .flatten

      val result = transformed.foldLeft(Map.empty[Long, Long].withDefault(_ => 0L)) { case (acc, (stone, count)) =>
        acc.updated(stone, acc(stone) + count)
      }
      blink(times - 1, result)
    }

  val result = blink(25, stones)
  println(result.values.sum) // 194782

  val result2 = blink(75, stones)
  println(result2.values.sum) // 233007586663131
