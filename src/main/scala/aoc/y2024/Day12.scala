package aoc.y2024

import aoc.data.{Direction, Pos}

import scala.annotation.tailrec

object Day12 extends Aoc2024("input_12.txt"):
  val grid = input.toGrid(identity)

  val posTuples = grid.toPosTuples

  def group(grid: Grid[Char], value: Char, start: Pos): Set[Pos] =
    @tailrec
    def _loop(queue: List[Pos], visited: Set[Pos]): Set[Pos] =
      queue match
        case Nil => visited
        case pos :: rest =>
          val neighbors = pos.adjacentNeighbors
            .filter(grid.isInside)
            .filter(p => grid(p.row)(p.col) == value)
            .filter(!visited.contains(_))
          _loop(rest ++ neighbors, visited ++ neighbors)

    _loop(List(start), Set(start))

  val grouped = posTuples
    .foldLeft(Set.empty[Set[Pos]]) { case (acc, (pos, value)) =>
      if acc.exists(_.contains(pos)) then acc
      else acc + group(grid, value, pos)
    }
    .toSeq

  def area(group: Set[Pos]): Int      = group.size
  def perimeter(group: Set[Pos]): Int = group.toSeq.flatMap(_.adjacentNeighbors.diff(group)).size
  def price(group: Set[Pos]): Int     = area(group) * perimeter(group)

  val result = grouped.map(price).sum
  println(result) // 1485656

  def countSides(group: Set[Pos]): Int =
    group.toSeq
      .map: pos =>
        val over =
          !group.contains(pos.move(Direction.Up)) && (!group.contains(pos.move(Direction.Left)) || group.contains(
            pos.move(Direction.Left).move(Direction.Up)
          ))

        val under =
          !group.contains(pos.move(Direction.Down)) && (!group.contains(pos.move(Direction.Left)) || group.contains(
            pos.move(Direction.Left).move(Direction.Down)
          ))

        val left =
          !group.contains(pos.move(Direction.Left)) && (!group.contains(pos.move(Direction.Down)) || group.contains(
            pos.move(Direction.Down).move(Direction.Left)
          ))

        val right =
          !group.contains(pos.move(Direction.Right)) && (!group.contains(pos.move(Direction.Down)) || group.contains(
            pos.move(Direction.Down).move(Direction.Right)
          ))

        Seq(over, under, left, right).count(identity)
      .sum

  val result2 = grouped.map(g => countSides(g) * area(g)).sum
  println(result2) //
