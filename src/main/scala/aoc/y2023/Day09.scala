package aoc.y2023

import scala.annotation.tailrec

object Day09 extends Aoc2023("input_09.txt"):
  val data = input.map(_.split(" ").map(_.toLong).toList)

  def untilZeroes(data: List[Long]): List[List[Long]] =
    @tailrec
    def untilZeroes0(data: List[Long], acc: List[List[Long]]): List[List[Long]] =
      if data.forall(_ == 0) then acc
      else
        val next = data.zip(data.tail).map((n1, n2) => n2 - n1)
        untilZeroes0(next, next :: acc)

    untilZeroes0(data, List(data))

  val result = data
    .map: d =>
      untilZeroes(d).map(_.last).sum
    .sum

  println(result)

  val result2 = data
    .map: d =>
      untilZeroes(d)
        .map(_.head)
        .foldLeft(List.empty[Long]): (acc, n) =>
          if acc.isEmpty then 0 :: acc
          else n - acc.head :: acc
    .map(_.head)
    .sum

  println(result2)
