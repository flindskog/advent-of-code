package aoc.y2024

import scala.annotation.tailrec

object Day05 extends Aoc2024("input_05.txt"):
  val List(rulesStr, dataStr) = input.splitByEmptyLine.toList

  val parseData: String => List[Int] =
    _.split(",").map(_.toInt).toList
  val data = dataStr.map(parseData)

  val parseRule: String => (Int, Int) = s =>
    val arr = s.split("""\|""")
    (arr(0).toInt, arr(1).toInt)
  val rules = rulesStr.map(parseRule)

  val rulesBefore = rules
    .groupBy((first, _) => first)
    .view
    .mapValues(_.map((_, last) => last).toSet)
    .toMap

  def isValid(data: List[Int]): Boolean = {
    @tailrec
    def loop(data: List[Int], seen: Set[Int]): Boolean = data match
      case Nil => true
      case head :: tail =>
        if rulesBefore.get(head).forall(value => seen.intersect(value).isEmpty) then loop(tail, seen + head)
        else false

    loop(data, Set.empty)
  }

  val (valid, invalid) = data.partition(isValid)

  val result = valid
    .map(l => l(l.length / 2))
    .sum

  println(result) // 5991

  val result2 = invalid
    .map: l =>
      val asSet = l.toSet

      given Ordering[Int] = Ordering.fromLessThan { (a, b) =>
        val aRules = rulesBefore.getOrElse(a, Set.empty).intersect(asSet)
        val bRules = rulesBefore.getOrElse(b, Set.empty).intersect(asSet)
        aRules.size > bRules.size
      }

      l.sorted.apply(l.length / 2)
    .sum

  println(result2) // 5479