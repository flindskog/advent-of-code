package aoc.y2024

import aoc.data.{UndirectedEdge, UndirectedGraph}

object Day23 extends Aoc2024("input_23.txt"):
  val pairs = input.map: line =>
    val Array(first, second) = line.split("-")
    (first, second)

  val connectedTo = pairs.foldLeft(Map[String, Set[String]]()) { case (acc, (first, second)) =>
    val firstSet  = acc.getOrElse(first, Set.empty)
    val secondSet = acc.getOrElse(second, Set.empty)

    acc.updated(first, firstSet + second).updated(second, secondSet + first)
  }

  val cyclesOfThree = connectedTo.flatMap { case (first, connected) =>
    connected.flatMap { second =>
      connectedTo(second).flatMap { third =>
        if connectedTo(third).contains(first) then Some(Set(first, second, third))
        else None
      }
    }
  }.toSet

  val tCycles = cyclesOfThree.filter(_.exists(_.startsWith("t")))
  println(tCycles.size) // 1358
  
  val vertexToCycles = cyclesOfThree.flatMap { cycle =>
    cycle.map(_ -> cycle)
  }.groupMap(_._1)(_._2)
  
  println(vertexToCycles)

  // Looks like we should use the Bron-Kerbosch algorithm to find maximal cliques for part two
  // https://en.wikipedia.org/wiki/Bron–Kerbosch_algorithm
