package aoc.y2024

import scala.annotation.tailrec
import scala.util.Try

object Day03 extends Aoc2024("input_03.txt"):

  val lineRegex = """mul\([0-9]{1,3},[0-9]{1,3}\)""".r
  val mulRegex  = """mul\(([0-9]{1,3}),([0-9]{1,3})\)""".r

  val doMul: String => Long = s =>
    Try {
      val mulRegex(a, b) = s: @unchecked
      a.toLong * b.toLong
    }.getOrElse(0L)

  val result1 = input
    .flatMap(lineRegex.findAllIn)
    .map(doMul)
    .sum

  println(result1) // 164730528

  @tailrec
  def clean(str: String, acc: String, collect: Boolean): String =
    str match
      case "" => acc
      case s"do()$rest" =>
        clean(rest, acc, true)
      case s"don't()$rest" =>
        clean(rest, acc, false)
      case _ =>
        if (collect) clean(str.tail, acc + str.head, true)
        else clean(str.tail, acc, false)

  val result2 = lineRegex
    .findAllIn(clean(input.mkString, "", true))
    .map(doMul)
    .sum

  println(result2) // 70478672
