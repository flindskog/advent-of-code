package aoc.data

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer

case class UndirectedEdge[T](a: T, b: T)

case class UndirectedGraph[T](edges: Set[UndirectedEdge[T]]) {

  val vertices: Set[T] = edges.flatMap(e => Set(e.a, e.b))

  val neighbours: Map[T, Set[T]] =
    edges.foldLeft(Map[T, Set[T]]()) { (acc, edge) =>
      acc
        .updatedWith(edge.a)(_.map(_ + edge.b).orElse(Some(Set(edge.b))))
        .updatedWith(edge.b)(_.map(_ + edge.a).orElse(Some(Set(edge.a))))
    }

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
            val unvisitedNeighbours = neighbours(vertex).diff(visited).diff(visited)
            val newQueue = restQueue.enqueueAll(unvisitedNeighbours.map(neighbour => (neighbour, distance + 1)))
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
            val unvisitedNeighbours = neighbours(head).diff(visited)
            loop(unvisitedNeighbours.toList ++ tail, visited + head, component + head)
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

  def maximalCliques: List[Set[T]] = {
    def bronKerbosch(
        r: Set[T],
        pStart: Set[T],
        xStart: Set[T],
        result: ListBuffer[Set[T]]
    ): Unit = {
      var p = pStart
      var x = xStart
      if (p.isEmpty && x.isEmpty) {
        result.append(r)
      } else {
        val pivot = p.union(x).head
        for (v <- p.diff(neighbours(pivot))) {
          val vNeighbours = neighbours(v)
          bronKerbosch(r + v, p.intersect(vNeighbours), x.intersect(vNeighbours), result)
          x += v
          p -= v
        }
      }
    }

    val result = ListBuffer[Set[T]]()
    bronKerbosch(Set(), vertices, Set(), result)
    result.toList
  }
}

object UndirectedGraph {
  def empty[T]: UndirectedGraph[T] = UndirectedGraph(Set())
}
