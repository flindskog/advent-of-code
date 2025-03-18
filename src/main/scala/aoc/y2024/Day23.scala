package aoc.y2024

import aoc.data.UndirectedGraph

object Day23 extends Aoc2024("input_23.txt"):
  val pairs = input.map: line =>
    val Array(first, second) = line.split("-")
    (first, second)

  val graph = pairs.foldLeft(UndirectedGraph.empty[String]) { case (acc, (first, second)) =>
    acc.addEdge(first, second)
  }

  val cyclesOfThree = graph.neighbours.flatMap { case (first, connected) =>
    connected.flatMap { second =>
      graph.neighbours(second).flatMap { third =>
        if graph.neighbours(third).contains(first) then Some(Set(first, second, third))
        else None
      }
    }
  }.toSet

  val tCycles = cyclesOfThree.filter(_.exists(_.startsWith("t")))
  println(tCycles.size) // 1358

  val cliques = graph.maximalCliques
  println(cliques.maxBy(_.size).toList.sorted.mkString(",")) // cl,ei,fd,hc,ib,kq,kv,ky,rv,vf,wk,yx,zf
