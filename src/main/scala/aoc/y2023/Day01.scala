package aoc.y2023

import aoc.utils.Input
import aoc.y2023.Day01_1.data
import aoc.y2023.Day01_2.data

trait Day01 {
  val data = Input.read("2023/input_01.txt")
}

object Day01_1 extends App with Day01 {
  val result =
    (data.map(_.find(_.isDigit)) zip data.map(_.findLast(_.isDigit))).flatMap { case (f, l) =>
      (f, l) match {
        case (Some(f), Some(l)) => Some((f.toString + l.toString).toInt)
        case _                  => None
      }
    }

  println(result.sum)
}

object Day01_2 extends App with Day01 {
  val numberRegex = "(?=(1|2|3|4|5|6|7|8|9|0|one|two|three|four|five|six|seven|eight|nine|zero))".r
  val numberMap = Map(
    "one"   -> "1",
    "two"   -> "2",
    "three" -> "3",
    "four"  -> "4",
    "five"  -> "5",
    "six"   -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine"  -> "9",
    "zero"  -> "0"
  )
  val result = data
    .map(s => numberRegex.findAllMatchIn(s).map(_.group(1)).toList)
    .map(r => (numberMap.getOrElse(r.head, r.head), numberMap.getOrElse(r.last, r.last)))
    .map { case (f, l) => (f + l).toInt }

  println(result.sum)
}
