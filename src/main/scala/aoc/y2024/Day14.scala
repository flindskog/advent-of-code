package aoc.y2024

import aoc.data.Pos

import scala.annotation.tailrec

object Day14 extends Aoc2024("input_14.txt"):
  val regex = """p=([0-9]+),([0-9]+) v=(-?[0-9]+),(-?[0-9]+)""".r

  case class Robot(position: Pos, velocity: Pos)

  def toRobot(str: String): Robot =
    str match
      case regex(pcol, prow, vcol, vrow) => Robot(Pos(prow.toInt, pcol.toInt), Pos(vrow.toInt, vcol.toInt))
      case _                             => sys.error(s"Invalid input: $str")

  val robots = input.map(toRobot)

  val spaceCols = 101
  val spaceRows = 103

  def move(robot: Robot): Robot =
    val newPos = robot.position + robot.velocity
    val row    = if newPos.row < 0 then spaceRows + newPos.row else newPos.row % spaceRows
    val col    = if newPos.col < 0 then spaceCols + newPos.col else newPos.col % spaceCols
    robot.copy(position = Pos(row, col))

  @tailrec
  def moveTimes(times: Int)(robot: Robot): Robot =
    if times == 0 then robot
    else moveTimes(times - 1)(move(robot))

  def quadrant(robot: Robot): Option[Int] =
    val row = robot.position.row
    val col = robot.position.col
    if row == (spaceRows / 2) || col == (spaceCols / 2) then None
    else if row < spaceRows / 2 && col < spaceCols / 2 then Some(1)
    else if row < spaceRows / 2 && col >= spaceCols / 2 then Some(2)
    else if row >= spaceRows / 2 && col < spaceCols / 2 then Some(3)
    else Some(4)

  val result = robots
    .map(moveTimes(100))
    .flatMap(quadrant)
    .groupMapReduce(identity)(_ => 1)(_ + _)
    .values
    .product

  println(result) // 228410028

  // A proper Christmas tree should have one robot on top, then three robots, then five, etc.
  def isChristmasTree(robotPositions: Set[Pos]): Boolean =
    def formRestOfTop(pos: Pos): Set[Pos] =
      val topTriangle = Set(Pos(1, 0), Pos(1, 1), Pos(1, -1), Pos(2, 0), Pos(2, 1), Pos(2, -1), Pos(2, -2), Pos(2, 2))
      topTriangle.map(_ + pos)

    robotPositions.exists(pos => formRestOfTop(pos).subsetOf(robotPositions))

  def findChristmasTree(robots: Seq[Robot]): Option[(Int, Set[Pos])] =
    LazyList
      .from(0)
      .zip(LazyList.iterate(robots)(_.map(move)))
      .map((times, movedRobots) => (times, movedRobots.map(_.position).toSet))
      .find((_, positions) => isChristmasTree(positions))

  findChristmasTree(robots) match
    case Some((times, positions)) =>
      println(Pos.drawAsGrid(positions))
      println(times) // 8258
    case None => sys.error("No Christmas tree found")
