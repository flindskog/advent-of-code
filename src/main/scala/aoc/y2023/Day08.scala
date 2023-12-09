package aoc.y2023

import aoc.utils
import aoc.utils.Input

import scala.annotation.tailrec

enum Direction:
  case Left, Right

case class Node(value: String, left: String, right: String)

object Day08 extends Aoc2023("input_08.txt"):
  val directionData =
    input.head.map {
      case 'L' => Direction.Left
      case 'R' => Direction.Right
      case d   => sys.error(s"Invalid direction [$d]")
    }.toList

  val directions = LazyList.continually(directionData).flatten
  val treeData   = input.drop(2)

  val regex = """(...) = \((...), (...)\)""".r
  val graph = treeData.map { case regex(value, left, right) =>
    value -> Node(value, left, right)
  }.toMap

  def nextNode(from: String, direction: Direction): String = {
    val node = graph(from)
    direction match
      case Direction.Left  => node.left
      case Direction.Right => node.right
  }

  @tailrec
  def find(from: String, to: String, directions: LazyList[Direction], acc: List[String]): List[String] =
    if from == to then to :: acc
    else
      val next = nextNode(from, directions.head)
      find(next, to, directions.tail, from :: acc)

  val result = find("AAA", "ZZZ", directions, Nil).reverse

  println(result.size - 1) // 14429

  val startNodes = graph.keySet.filter(_.endsWith("A"))

  def zPath(from: String, directions: LazyList[Direction], step: Int = 0): LazyList[(String, Int)] = {
    val next = nextNode(from, directions.head)
    (from, step) #:: zPath(next, directions.tail, step + 1)
  }

  val zDistances = startNodes
    .flatMap(node => zPath(node, directions).filter { case (node, _) => node.endsWith("Z") }.take(1))
    .map((_, step) => step.toLong)
    .toList

  // By ocular inspection it could be figured out that the path is a loop!
  // Solving problems by coincidence!!!
  println(utils.Math.lcm(zDistances)) // 10921547990923
