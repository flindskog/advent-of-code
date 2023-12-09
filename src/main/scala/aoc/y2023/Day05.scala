package aoc.y2023

import aoc.utils.Input

import scala.annotation.tailrec

case class Range(dstStart: Long, srcStart: Long, length: Long):
  def destination(source: Long): Option[Long] =
    if source >= srcStart && source < srcStart + length
    then Some(dstStart + (source - srcStart))
    else None

  def source(destination: Long): Option[Long] =
    if destination >= dstStart && destination < dstStart + length
    then Some(srcStart + (destination - dstStart))
    else None

case class Mapping(srcCat: String, dstCat: String, ranges: Seq[Range]):
  def destination(source: Long): Long =
    ranges.find(_.destination(source).isDefined).map(_.destination(source).get).getOrElse(source)

  def source(destination: Long): Long =
    ranges.find(_.source(destination).isDefined).map(_.source(destination).get).getOrElse(destination)

object Day05 extends Aoc2023("input_05.txt"):
  val data = input
    .foldLeft(Seq(Seq.empty[String])): (acc, line) =>
      if line.isEmpty then acc :+ Seq()
      else acc.init :+ (acc.lastOption.getOrElse(Seq()) :+ line)

  val seeds = data.head.head.split(" ").toList.tail.map(_.toLong)

  val mappings = data.tail
    .map: lines =>
      val regex = """(\w+)-to-(\w+) map:""".r
      val (srcCat, dstCat) = lines.head match
        case regex(src, dst) => (src, dst)

      val ranges = lines.tail.map: line =>
        val regex = """(\d+) (\d+) (\d+)""".r
        val (dstStart, srcStart, length) = line match
          case regex(dstStart, srcStart, length) => (dstStart, srcStart, length)
        Range(dstStart.toLong, srcStart.toLong, length.toLong)

      (srcCat, Mapping(srcCat, dstCat, ranges))
    .toMap

  val reverseMappings = mappings.map: (_, mapping) =>
    (mapping.dstCat, mapping)

  @tailrec
  final def mapTo(src: String, dst: String, seeds: List[Long]): List[Long] = {
    val mapping = mappings(src)
    val mapped  = seeds.map(mapping.destination)
    if mapping.dstCat == dst then mapped
    else mapTo(mapping.dstCat, dst, mapped)
  }

  @tailrec
  final def reverseMap(src: String, dst: String, n: Long): Long =
    if src == dst then n
    else
      val mapping = reverseMappings(src)
      val mapped  = mapping.source(n)
      reverseMap(mapping.srcCat, dst, mapped)

  val mapped = mapTo("seed", "location", seeds)
  println(mapped.min)

  val rangeSeeds = seeds
    .grouped(2)
    .map:
      case Seq(a, b) => (a, b)
      case v         => sys.error(s"Unexpected grouping $v")
    .toSeq
  val infiniteSeq: LazyList[Long] = {
    def loop(v: Long): LazyList[Long] = v #:: loop(v + 1)
    loop(0)
  }

  // Could be improved by sorting and binary search
  def isInSeed(n: Long): Boolean = rangeSeeds.exists { case (a, b) => a <= n && n <= a + b }

  val result = infiniteSeq
    .map(n => (n, reverseMap("location", "seed", n)))
    .find((_, seed) => isInSeed(seed))

  result match
    case Some((n, _)) => println(n) // 28580589
    case None         => println("Not found")
