package aoc.y2023

import scala.collection.parallel.CollectionConverters._

object Day12 extends Aoc2023("input_12.txt"):

  def compact(pattern: String): String =
    pattern.replaceAll("^\\.+", "").replaceAll("\\.+", ".").replaceAll("\\.+$", "")

  def parse(dupFactor: Int)(line: String): (Seq[Char], Seq[Int]) =
    val Array(p, c)  = line.split(" ")
    val duplicated   = List.fill(dupFactor)(p).mkString("?")
    val pattern      = compact(duplicated).toSeq
    val combinations = List.fill(dupFactor)(c.split(",").toList.map(_.toInt)).flatten
    (pattern, combinations)

  def count(pattern: Seq[Char], combos: Seq[Int]): Long = {
    val memo = scala.collection.mutable.Map[(Seq[Char], Seq[Int]), Long]()

    def count0(pattern: Seq[Char], combos: Seq[Int]): Long =
      memo.get((pattern, combos)) match
        case Some(value) => value
        case None =>
          val result: Long = (pattern, combos) match
            case (pattern, combos) if pattern.length < combos.sum + combos.size - 1 => 0 // Cannot match
            case (Seq(), combos) =>
              if combos.isEmpty then 1
              else 0 // unmatched
            case (pattern, Seq()) =>
              if pattern.contains('#') then 0 // unmatched
              else 1
            case (pattern, combos) if pattern.head == '.' => // Starting with a dot, we can drop it
              count0(pattern.tail, combos)
            case (pattern, combos) if pattern.head == '#' => // check if we can match the first combo
              val size         = combos.head
              val (part, next) = pattern.splitAt(size)
              if part.size < size || part.contains('.') || next.headOption.contains('#') then 0 // Cannot match
              else                                                                              // Matched
                next match
                  case Seq() => count0(Seq(), combos.tail)
                  case next  => count0(next.tail, combos.tail)
            case (pattern, combos) if pattern.head == '?' => // Wildcard - branch the two possibilities
              count0('.' +: pattern.tail, combos) + count0('#' +: pattern.tail, combos)

          memo.put((pattern, combos), result)
          result

    count0(pattern, combos)
  }
  println(input.par.map(parse(1)).map(count).sum) // 7460
  println(input.par.map(parse(5)).map(count).sum) // 6720660274964
