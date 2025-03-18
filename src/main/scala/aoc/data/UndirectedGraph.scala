package aoc.data

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class UndirectedEdge[T](a: T, b: T)

case class UndirectedGraph[T](edges: Set[UndirectedEdge[T]]) {

  def addEdge(vertex1: T, vertex2: T): UndirectedGraph[T] =
    UndirectedGraph(edges + UndirectedEdge(vertex1, vertex2))

  def addEdges(vertex: T, verticess: Set[T]): UndirectedGraph[T] =
    verticess.foldLeft(this)((graph, vertex2) => graph.addEdge(vertex, vertex2))

  def removeEdge(vertex1: T, vertex2: T): UndirectedGraph[T] =
    UndirectedGraph(edges - UndirectedEdge(vertex1, vertex2))

  def depthFirstTraversal(start: T): List[T] = {
    @tailrec
    def loop(stack: List[T], visited: Set[T], path: Vector[T]): Vector[T] =
      stack match {
        case Nil => path
        case head :: tail =>
          if (visited.contains(head)) loop(tail, visited, path)
          else {
            val neighbours = edges
              .filter(edge => edge.a == head || edge.b == head)
              .map(edge => if (edge.a == head) edge.b else edge.a)
              .diff(visited)
            loop(neighbours.toList ++ tail, visited + head, path :+ head)
          }
      }

    loop(List(start), Set(), Vector()).toList
  }

  def shortestDistance(from: T, to: T): Int = {
    @tailrec
    def loop(queue: Queue[(T, Int)], visited: Set[T]): Int =
      queue.dequeue match {
        case ((vertex, distance), restQueue) =>
          if (vertex == to) distance
          else {
            val neighbours = edges
              .filter(edge => edge.a == vertex || edge.b == vertex)
              .map(edge => if (edge.a == vertex) edge.b else edge.a)
              .diff(visited)
            val newQueue = restQueue.enqueueAll(neighbours.map(neighbour => (neighbour, distance + 1)))
            loop(newQueue, visited + vertex)
          }
      }

    loop(Queue((from, 0)), Set())
  }

  def localBridges: List[(UndirectedEdge[T], Int)] =
    edges.map { edge =>
      val graph    = removeEdge(edge.a, edge.b)
      val distance = graph.shortestDistance(edge.a, edge.b)
      (edge, distance)
    }.toList.filter((_, span) => span > 2)

  def components: List[Set[T]] = {
    @tailrec
    def loop(stack: List[T], visited: Set[T], component: Set[T]): Set[T] =
      stack match {
        case Nil => component
        case head :: tail =>
          if (visited.contains(head)) loop(tail, visited, component)
          else {
            val neighbours = edges
              .filter(edge => edge.a == head || edge.b == head)
              .map(edge => if (edge.a == head) edge.b else edge.a)
              .diff(visited)
            loop(neighbours.toList ++ tail, visited + head, component + head)
          }
      }

    @tailrec
    def loop2(vertices: Set[T], components: List[Set[T]]): List[Set[T]] =
      if (vertices.isEmpty) components
      else {
        val component = loop(List(vertices.head), Set(), Set())
        loop2(vertices.diff(component), component :: components)
      }

    loop2(edges.flatMap(edge => Set(edge.a, edge.b)), List())
  }


  /**
   * Finds the largest cliques with the BronKerbosch algorithm
   */
  def largestClique: List[Set[T]] = {
    @tailrec
    def bronKerbosch(
        r: Set[T],
        p: Set[T],
        x: Set[T],
        cliques: List[Set[T]]
    ): List[Set[T]] =
      if (p.isEmpty && x.isEmpty) r :: cliques
      else {
        val pivot = p.union(x).maxBy(vertex => p.intersect(edges(vertex)).size)
        val newR  = r + pivot
        val newP  = p.intersect(edges(pivot))
        val newX  = x.intersect(edges(pivot))
        bronKerbosch(newR, newP, newX, cliques) ++
          bronKerbosch(r, p - pivot, x + pivot, cliques)
      }

    bronKerbosch(Set(), edges.flatMap(edge => Set(edge.a, edge.b)), Set(), List())
  }
}

object UndirectedGraph {
  def empty[T]: UndirectedGraph[T] = UndirectedGraph(Set())
}
