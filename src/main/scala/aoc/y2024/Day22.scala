package aoc.y2024

import scala.annotation.tailrec

object Day22 extends Aoc2024("input_22.txt"):
  val numbers = input.map(_.toLong)

  def mix(n1: Long, n2: Long): Long = n1 ^ n2
  def prune(n: Long): Long          = n % 16777216

  def nextSecret(n: Long): Long =
    val r1 = prune(mix(n, n * 64))
    val r2 = prune(mix(r1, r1 / 32))
    val r3 = prune(mix(r2, r2 * 2048))

    r3

  def nSecrets(n: Int)(secret: Long): Seq[Long] =
    @tailrec
    def loop(n: Int, secret: Long, acc: Seq[Long]): Seq[Long] =
      if n == 0 then acc
      else
        val next = nextSecret(secret)
        loop(n - 1, next, acc :+ next)

    loop(n, secret, Vector(secret))

  val result = numbers.map(nSecrets(2000)(_).last).sum
  println(result) // 17724064040

  type BuyTrigger = (Int, Int, Int, Int)

  def findChanges(secrets: Seq[Long]): Map[BuyTrigger, Int] =
    secrets
      .map(s => (s % 10).toInt)
      .sliding(5)
      .map { case Seq(a, b, c, d, e) =>
        (b - a, c - b, d - c, e - d) -> e
      }
      .toSeq
      .groupBy(_._1)
      .map((k, v) => k -> v.head._2)

  def merge(m1: Map[BuyTrigger, Int], m2: Map[BuyTrigger, Int]): Map[BuyTrigger, Int] =
    m1 ++ m2.map: (k, v) =>
      k -> (v + m1.getOrElse(k, 0))

  val secretSeq = numbers.map(nSecrets(2000))
  val changeMap = secretSeq.map(findChanges)
  val merged    = changeMap.reduce(merge)
  println(merged.values.max) // 1998
