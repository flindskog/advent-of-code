import util.Input

import scala.annotation.tailrec

case class Range(dstStart: Long, srcStart: Long, length: Long) {
  def destination(source: Long): Option[Long] =
    if (source >= srcStart && source < srcStart + length) {
      Some(dstStart + (source - srcStart))
    } else {
      None
    }

  def source(destination: Long): Option[Long] =
    if (destination >= dstStart && destination < dstStart + length) {
      Some(srcStart + (destination - dstStart))
    } else {
      None
    }
}

case class Mapping(srcCat: String, dstCat: String, ranges: Seq[Range]) {
  def destination(source: Long): Long =
    ranges.find(_.destination(source).isDefined).map(_.destination(source).get).getOrElse(source)

  def source(destination: Long): Long =
    ranges.find(_.source(destination).isDefined).map(_.source(destination).get).getOrElse(destination)
}

trait Day05 {
  val data = Input.read("input_05.txt").foldLeft(Seq(Seq.empty[String])) { (acc, line) =>
    if (line.isEmpty) acc :+ Seq()
    else acc.init :+ (acc.lastOption.getOrElse(Seq()) :+ line)
  }

  val seeds = data.head.head.split(" ").toList.tail.map(_.toLong)

  val mappings = data.tail.map { lines =>
    val regex                 = """(\w+)-to-(\w+) map:""".r
    val regex(srcCat, dstCat) = lines.head
    val ranges = lines.tail.map { line =>
      val regex                             = """(\d+) (\d+) (\d+)""".r
      val regex(dstStart, srcStart, length) = line
      Range(dstStart.toLong, srcStart.toLong, length.toLong)
    }
    (srcCat, Mapping(srcCat, dstCat, ranges))
  }.toMap

  val reverseMappings = mappings.map { case (_, mapping) =>
    (mapping.dstCat, mapping)
  }

  @tailrec
  final def mapTo(src: String, dst: String, seeds: List[Long]): List[Long] = {
    val mapping = mappings(src)
    val mapped  = seeds.map(mapping.destination)
    if (mapping.dstCat == dst) mapped
    else mapTo(mapping.dstCat, dst, mapped)
  }

  @tailrec
  final def reverseMap(src: String, dst: String, n: Long): Long =
    if (src == dst) n
    else {
      val mapping = reverseMappings(src)
      val mapped  = mapping.source(n)
      reverseMap(mapping.srcCat, dst, mapped)
    }
}

object Day05_1 extends App with Day05 {
  val mapped = mapTo("seed", "location", seeds)
  println(mapped.min)
}

object Day05_2 extends App with Day05 {
  val rangeSeeds = seeds.grouped(2).map { case Seq(a, b) => (a, b) }.toSeq
  val infiniteSeq: LazyList[Long] = {
    def loop(v: Long): LazyList[Long] = v #:: loop(v + 1)
    loop(0)
  }

  // Could be improved by sorting and binary search
  def isInSeed(n: Long): Boolean = rangeSeeds.exists { case (a, b) => a <= n && n <= a + b }

  val result = infiniteSeq
    .map(n => (n, reverseMap("location", "seed", n)))
    .find((_, seed) => isInSeed(seed))

  result match {
    case Some((n, _)) => println(n) // 28580589
    case None         => println("Not found")
  }
}