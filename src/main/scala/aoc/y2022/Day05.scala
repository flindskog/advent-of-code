package aoc.y2022

import scala.annotation.tailrec
import scala.util.Try

object Day05 extends Aoc2022("input_05.txt"):
  val splitted = input.splitByEmptyLine
  val stackData = splitted.head.init.reverse.toIndexedSeq.map: line =>
    val regex = """([ ]{3}|\[\w])(?:$| )""".r
    regex
      .findAllIn(line)
      .map { case regex(value) =>
        value
      }
      .map: value =>
        if value == "   " then None
        else Some(value.charAt(1))
      .toIndexedSeq

  val stacks = stackData.head.zipWithIndex.map: (_, index) =>
    @tailrec
    def fill(col: Int, row: Int = 0, acc: List[Char] = Nil): List[Char] =
      val value = Try(stackData(row)(col)).toOption.flatten
      value match
        case Some(value) =>
          fill(col, row + 1, value :: acc)
        case None =>
          acc
    fill(index)

  val moves = splitted.last.flatMap: line =>
    val regex = """move (\d+) from (\d+) to (\d+)""".r
    regex
      .findFirstIn(line)
      .map:
        case regex(n, from, to) =>
          (n.toInt, from.toInt - 1, to.toInt - 1)

  def move(batched: Boolean)(n: Int, from: Int, to: Int, stacks: IndexedSeq[List[Char]]): IndexedSeq[List[Char]] =
    val fromStack = stacks(from)
    if fromStack.size < n then throw new RuntimeException(s"fromStack [$from] $fromStack is smaller than $n")
    val toStack = stacks(to)
    val toMove  = if batched then fromStack.take(n) else fromStack.take(n).reverse
    stacks
      .updated(from, fromStack.drop(n))
      .updated(to, toMove ::: toStack)

  val res = moves.foldLeft(stacks) { case (stacks, (n, from, to)) =>
    move(false)(n, from, to, stacks)
  }

  println(res.map(_.head).mkString("")) // MQTPGLLDN

  val res2 = moves.foldLeft(stacks) { case (stacks, (n, from, to)) =>
    move(true)(n, from, to, stacks)
  }

  println(res2.map(_.head).mkString("")) // LVZPSTTCZ
