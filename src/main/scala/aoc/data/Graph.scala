package aoc.data

import scala.annotation.tailrec

case class Graph[T](vertices: Set[T], edges: Map[T, Set[T]]) {
  def addEdge(from: T, to: T): Graph[T] =
    val newEdges = edges + (from -> (edges.getOrElse(from, Set()) + to))
    Graph(vertices + from + to, newEdges)

  def addEdges(from: T, tos: Set[T]): Graph[T] =
    val newEdges = edges + (from -> (edges.getOrElse(from, Set()) ++ tos))
    Graph(vertices + from ++ tos, newEdges)

  def addVertex(vertex: T): Graph[T] =
    Graph(vertices + vertex, edges)

  def addVertices(vertices: Set[T]): Graph[T] =
    Graph(this.vertices ++ vertices, edges)

  def removeEdge(from: T, to: T): Graph[T] =
    val newEdges = edges + (from -> (edges.getOrElse(from, Set()) - to))
    Graph(vertices, newEdges)

  def removeVertex(vertex: T): Graph[T] =
    val newEdges = edges.map { case (from, tos) => (from, tos - vertex) }
    Graph(vertices - vertex, newEdges)

  def removeVertices(vertices: Set[T]): Graph[T] =
    val newEdges = edges.map { case (from, tos) => (from, tos -- vertices) }
    Graph(this.vertices -- vertices, newEdges)

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
  def empty[T]: Graph[T] = Graph(Set(), Map())
}
