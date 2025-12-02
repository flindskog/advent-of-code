package aoc.y2025

import scala.annotation.tailrec

object Day02 extends Aoc2025("input_02.txt"):
  case class Range(start: Long, end: Long) {
    def filter(f: Long => Boolean): Seq[Long] = {
      @tailrec
      def loop(s: Long, acc: Seq[Long]): Seq[Long] =
        if s > end then acc
        else if f(s) then loop(s + 1, acc :+ s)
        else loop(s + 1, acc)

      loop(start, Seq.empty)
    }
  }

  val ranges = input.head.split(",").map { range =>
    range.split("-") match {
      case Array(start, end) => Range(start.toLong, end.toLong)
    }
  }

  def illegalNumbers(range: Range): Seq[Long] =
    range.filter { s =>
      val str = s.toString
      str.length % 2 == 0 && {
        val (first, second) = str.splitAt(str.length / 2)
        first == second
      }
    }

  val result1 = ranges.flatMap(illegalNumbers).sum
  println(result1)

  def illegal(number: String): Boolean = {
    @tailrec
    def loop(size: Int): Boolean =
      if size > number.length / 2 then false
      else if number.length % size == 0 then
        val parts = number.grouped(size).toSet
        if parts.size == 1 then true
        else loop(size + 1)
      else loop(size + 1)

    loop(1)
  }

  def illegalNumbers2(range: Range): Seq[Long] =
    range.filter(s => illegal(s.toString))

  val result2 = ranges.flatMap(illegalNumbers2).sum
  println(result2)