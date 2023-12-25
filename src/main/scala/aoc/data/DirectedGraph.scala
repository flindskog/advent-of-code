package aoc.data

import scala.annotation.tailrec

case class DirectedEdge[T, W](from: T, to: T)

case class DirectedGraph[T](edges: Set[DirectedEdge[T, T]]) {
  val edgesByKey: Map[T, Set[DirectedEdge[T, T]]] = edges.groupBy(_.from)
  val vertices                                    = edgesByKey.keySet

  def addEdge(from: T, to: T): DirectedGraph[T] = DirectedGraph(edges + DirectedEdge(from, to))

  def inverse: DirectedGraph[T] = DirectedGraph(edges.map(e => DirectedEdge(e.to, e.from)))

  def bfs(start: T): List[T] = {
    @tailrec
    def loop(queue: List[T], visited: Set[T], acc: List[T]): List[T] =
      queue match {
        case Nil => acc.reverse
        case h :: t =>
          if (visited.contains(h)) loop(t, visited, acc)
          else {
            val newVisited = visited + h
            val newQueue   = t ++ edgesByKey.getOrElse(h, Set.empty).map(_.to).diff(newVisited)
            loop(newQueue, newVisited, h :: acc)
          }
      }

    loop(List(start), Set.empty, Nil)
  }
}

object DirectedGraph {
  def empty[T]: DirectedGraph[T] = DirectedGraph(Set.empty)
}
