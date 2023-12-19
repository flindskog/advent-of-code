package aoc.data

import scala.annotation.tailrec

final class Graph[T](private val edges: Map[T, Set[T]]) {
  def addEdge(from: T, to: T): Graph[T] =
    val newEdges = edges + (from -> (edges.getOrElse(from, Set()) + to))
    Graph(newEdges)

  def addEdges(from: T, tos: Set[T]): Graph[T] =
    val newEdges = edges + (from -> (edges.getOrElse(from, Set()) ++ tos))
    Graph(newEdges)

  def removeEdge(from: T, to: T): Graph[T] =
    val newEdges = edges + (from -> (edges.getOrElse(from, Set()) - to))
    Graph(newEdges)

  def removeVertex(vertex: T): Graph[T] =
    val newEdges = edges.map { case (from, tos) => (from, tos - vertex) }
    Graph(newEdges - vertex)

  def removeVertices(vertices: Set[T]): Graph[T] =
    val newEdges = edges.map { case (from, tos) => (from, tos -- vertices) }
    Graph(newEdges)

  def depthFirstTraversal(start: T): List[T] = {
    @tailrec
    def loop(stack: List[T], visited: Set[T], path: Vector[T]): Vector[T] =
      stack match
        case Nil => path
        case vertex :: tail =>
          val newPath    = path :+ vertex
          val newVisited = visited + vertex
          val newStack   = tail ++ edges(vertex).diff(newVisited)
          loop(newStack, newVisited, newPath)

    loop(List(start), Set(), Vector()).toList
  }
}

object Graph {
  def empty[T]: Graph[T] = Graph(Map())
}
