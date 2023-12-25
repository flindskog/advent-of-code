package aoc.y2023

import aoc.data.UndirectedGraph

object Day25 extends Aoc2023("input_25.txt"):
  val graph = input.map { line =>
    val regex = """^(\w{3}): (.*)$""".r
    line match
      case regex(key, value) => key -> value.split(" ").toSet
  }.foldLeft(UndirectedGraph.empty[String]) { case (acc, (key, values)) =>
    acc.addEdges(key, values)
  }

  val bridges = graph.localBridges.sortBy(_._2).reverse

  val disconnectedGraph = bridges.take(3).map(_._1).foldLeft(graph) { case (acc, key) =>
    acc.removeEdge(key.a, key.b)
  }

  val components = disconnectedGraph.components

  val res = components.map(_.size).product
  println(res) // 555856
  // [total 189086ms] TODO: optimize
