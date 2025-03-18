package aoc.y2024

import scala.collection.mutable

object Day19 extends Aoc2024("input_19.txt"):
  val (towelString, designs) = input.splitByEmptyLine match
    case LazyList(t #:: LazyList(), d) => t -> d
    case _                             => sys.error("Invalid input")

  val towelsByLength  = towelString.split(", ").toSet.groupMap(_.length)(identity)
  val rTowelsByLength = towelsByLength.view.mapValues(_.filter(_.contains('r'))).filter(_._2.nonEmpty).toMap

  def designStartsWithTowels(design: String): Seq[String] =
    towelsByLength
      .flatMap: (length, towels) =>
        val start = design.take(length)
        Some(start).filter(towels.contains)
      .toSeq
      .sortBy(s => -s.length)

  val cache = mutable.Map[String, Boolean]()
  def canMatch(design: String): Boolean =
    def canMatchUncached(str: String): Boolean =
      if str.isEmpty then true
      else
        val matchingTowels = designStartsWithTowels(str)
        matchingTowels.foldLeft(false) { case (acc, towel) =>
          acc || canMatch(str.drop(towel.length))
        }

    cache.get(design) match
      case Some(value) => value
      case None =>
        val result = canMatchUncached(design)
        cache(design) = result
        result

  val designsThatCanMatch = designs.filter(canMatch)

  println(designsThatCanMatch.size) // 322

  val matchCache = mutable.Map[String, Seq[Seq[String]]]()
  def getAllMatches(design: String): Seq[Seq[String]] =
    def getAllMatchesUncached(str: String): Seq[Seq[String]] =
      if str.isEmpty then Seq(Seq.empty)
      else
        val matchingTowels = designStartsWithTowels(str)
        println(s"""GetAllMatches for $design: ${matchingTowels.mkString(",")}""")
        matchingTowels.flatMap { towel =>
          getAllMatches(str.drop(towel.length)).map(towel +: _)
        }

    matchCache.get(design) match
      case Some(value) => value
      case None =>
        val result = getAllMatchesUncached(design)
        matchCache(design) = result
        result

  val allMatches = designs.map(d => d -> getAllMatches(d))

  println(allMatches.map(_.size).sum) //
