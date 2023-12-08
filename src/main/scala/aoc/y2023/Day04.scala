package aoc.y2023

import aoc.utils.Input

trait Day04 {
  val data  = Input.read("2023/input_04.txt")
  val regex = """Card +\d+: +([\d ]+) \| +([\d ]+)""".r

  val wins = data.flatMap(s =>
    regex
      .findAllMatchIn(s)
      .map { m =>
        val winning = m.group(1).split(" +").map(_.toInt).toSet
        val my      = m.group(2).split(" +").map(_.toInt).toSet
        (winning intersect my).size
      }
      .toList
  )
}

object Day04_1 extends App with Day04 {
  val points = wins.map(w => Math.pow(2, w - 1).toInt)
  println(points.sum)
}

object Day04_2 extends App with Day04 {
  val acc = (Map[Int, Int]().withDefault(_ => 0), Map[Int, Int]().withDefault(_ => 0))
  val (result, _) = wins.zipWithIndex.foldLeft(acc) { case ((accWins, futureWins), (wins, idx)) =>
    val multiplier = 1 + futureWins(idx)
    val newAcc     = accWins.updated(idx, multiplier)
    val newFuture =
      if (wins == 0)
        futureWins
      else
        ((idx + 1) to (idx + wins)).foldLeft(futureWins) { case (acc, i) =>
          acc.updated(i, acc.getOrElse(i, 0) + multiplier)
        }
    (newAcc, newFuture)
  }

  println(result.values.sum)
}
