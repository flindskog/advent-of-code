package aoc.y2023

import aoc.utils.{Matrix, Text}
import com.sun.org.apache.xalan.internal.xsltc.compiler.util.Type

import scala.annotation.tailrec

object Day13 extends Aoc2023("input_13.txt"):
  def findReflection(lines: Seq[String]): Int =
    val potential = lines.zipWithIndex
      .zip(lines.tail)
      .filter { case ((currLine, _), nextLine) => currLine == nextLine }
      .map { case ((_, i), _) => i }

    potential.foldLeft(0): (acc, p) =>
      val (upper, lower) = lines.splitAt(p + 1)
      val upper2         = upper.reverse
      if upper2.startsWith(lower) || lower.startsWith(upper2) then p + 1 + acc
      else 0 + acc

  def findFuzzyReflection(lines: Seq[String]): Int =
    val potential = lines.zipWithIndex
      .zip(lines.tail)
      .map { case ((currLine, i), nextLine) => (i, currLine.levenshteinDistance(nextLine)) }
      .filter { case (_, ld) => ld <= 1 }

    potential
      .map: (p, ld) =>
        val (upper, lower) = lines.splitAt(p + 1)
        val upper2         = upper.reverse.tail
        val lower2         = lower.tail
        (p + 1, ld + upper2.zip(lower2).map((a, b) => a.levenshteinDistance(b)).sum)
      .filter((_, ld) => ld == 1)
      .map((p, _) => p)
      .headOption
      .getOrElse(0)

  val result1 = input.splitByEmptyLine.map: lines =>
    val rotated = lines.map(_.toSeq).rotateRight.map(_.mkString)
    (findReflection(lines), findReflection(rotated))

  println(result1.map((a, b) => a * 100 + b).sum) // 28895

  val result2 = input.splitByEmptyLine.map: lines =>
    val rotated = lines.map(_.toSeq).rotateRight.map(_.mkString)
    (findFuzzyReflection(lines), findFuzzyReflection(rotated))

  println(result2.map((a, b) => a * 100 + b).sum) // 31603
