package aoc.y2022

import aoc.data.{Direction, Pos}

import scala.util.Try

object Day09 extends Aoc2022("input_09.txt"):

  val directions = input.flatMap: line =>
    val Array(d, s) = line.split(" ")
    val direction = d match
      case "U" => Direction.Up
      case "D" => Direction.Down
      case "L" => Direction.Left
      case "R" => Direction.Right

    val steps = s.toInt
    LazyList.fill(steps)(direction)

  def moveTail(tail: Pos, head: Pos): Pos =
    if tail == head || tail.isTouching(head) then tail
    else
      val delta = head - tail
      tail + Pos(Try(delta.row / math.abs(delta.row)).getOrElse(0), Try(delta.col / math.abs(delta.col)).getOrElse(0))

  def moveRope(knots: Int): Vector[Vector[Pos]] =
    val init = Vector(Vector.fill(knots)(Pos.origo))
    directions.foldLeft(init): (acc, d) =>
      val head    = acc.last.head
      val tail    = acc.last.tail
      val newHead = head.move(d)
      val (_, newTail) = tail.foldLeft((newHead, Vector.empty[Pos])): (acc, t) =>
        val (toFollow, accTail) = acc
        val newTail             = moveTail(t, toFollow)
        (newTail, accTail :+ newTail)
      acc :+ (newHead +: newTail)

  val result  = moveRope(2).map(_.last).toSet.size
  val result2 = moveRope(10).map(_.last).toSet.size

  println(result)  // 5878
  println(result2) // 2405
