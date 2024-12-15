package aoc.y2024

import aoc.data.Pos.drawAsGrid
import aoc.data.{Direction, Pos}

import scala.annotation.tailrec

object Day15 extends Aoc2024("input_15.txt"):
  enum Type:
    case Robot, Box, Wall

  val debug              = false
  def println()          = if debug then Predef.println()
  def println(s: => Any) = if debug then Predef.println(s)

  val splitted = input.splitByEmptyLine

  val grid = splitted.head.map(_.toCharArray)
  val data = grid.indices
    .flatMap: row =>
      grid(row).indices.flatMap: col =>
        grid(row)(col) match
          case '#' => Some(Pos(row, col) -> Type.Wall)
          case 'O' => Some(Pos(row, col) -> Type.Box)
          case '@' => Some(Pos(row, col) -> Type.Robot)
          case _   => None
    .toMap

  val moves = splitted.last.flatMap: line =>
    line.map {
      case '^' => Direction.Up
      case 'v' => Direction.Down
      case '<' => Direction.Left
      case '>' => Direction.Right
    }

  val startPosition = data.find((_, _type) => _type == Type.Robot) match
    case Some((pos, _)) => pos
    case None           => throw new Exception("No robot found")

  // Tries to push a box in the given direction.
  // Returns the positions of everything that could be moved
  def push(pos: Pos, dir: Direction, data: Map[Pos, Type]): Set[Pos] =
    @tailrec
    def tryPush(pos: Pos, dir: Direction, acc: Set[Pos]): Set[Pos] =
      val nextPos = pos.move(dir)
      data.get(nextPos) match
        case Some(Type.Wall) =>
          Set.empty
        case Some(Type.Box) =>
          tryPush(nextPos, dir, acc + pos)
        case _ =>
          acc + pos

    tryPush(pos, dir, Set.empty)

  val result = moves.foldLeft((startPosition, data)) { case ((robotPos, data), moveDirection) =>
    val movedPositions = push(robotPos, moveDirection, data)
    val movedData      = movedPositions.map(pos => pos.move(moveDirection) -> data(pos)).toMap
    val newData        = data -- movedPositions ++ movedData
    val newRobotPos    = if (movedPositions.nonEmpty) robotPos.move(moveDirection) else robotPos
    println(moveDirection)
    print(newData)
    (newRobotPos, newData)
  }

  def print(data: Map[Pos, Type]) =
    println {
      Pos.drawAsGrid(data.groupMap { (_, t) =>
        t match
          case Type.Robot => '@'
          case Type.Box   => 'O'
          case Type.Wall  => '#'
      }(_._1).view.mapValues(_.toSet).toMap)
    }
    println()

  val coordinates = result._2.filter(_._2 == Type.Box).keys.map(p => p.row * 100 + p.col).sum
  Predef.println(coordinates) // 1398947

  enum Type2:
    case Wall, BoxLeft, BoxRight, Robot

  val data2 = grid.indices
    .flatMap: row =>
      grid(row).indices
        .flatMap: col =>
          val newCol       = col * 2
          val newPositions = Seq(Pos(row, newCol), Pos(row, newCol + 1))
          grid(row)(col) match
            case '#' => newPositions.zip(Seq(Type2.Wall, Type2.Wall))
            case 'O' => newPositions.zip(Seq(Type2.BoxLeft, Type2.BoxRight))
            case '@' => Seq(Pos(row, newCol) -> Type2.Robot)
            case _   => Seq.empty
    .toMap

  val startPosition2 = data2.find((_, _type) => _type == Type2.Robot) match
    case Some((pos, _)) => pos
    case None           => throw new Exception("No robot found")

  def print2(data: Map[Pos, Type2]) =
    println {
      Pos.drawAsGrid(data.groupMap { (_, t) =>
        t match
          case Type2.Robot    => '@'
          case Type2.BoxLeft  => '['
          case Type2.BoxRight => ']'
          case Type2.Wall     => '#'
      }(_._1).view.mapValues(_.toSet).toMap)
    }
    println()

  def push2(pos: Set[Pos], dir: Direction, data: Map[Pos, Type2]): Set[Pos] =
    @tailrec
    def tryPush(pos: Set[Pos], dir: Direction, acc: Set[Pos]): Set[Pos] =
      val nextPos = pos.map(_.move(dir))
      val d       = nextPos.flatMap(pos => data.get(pos).map(d => pos -> d)).toMap

      if d.isEmpty then acc ++ pos
      else if d.values.toSet.contains(Type2.Wall) then Set.empty
      else if dir == Direction.Left || dir == Direction.Right then tryPush(nextPos, dir, acc ++ pos)
      else
        val expanded = d
          .flatMap: (pos, t) =>
            if t == Type2.BoxLeft then Set(pos, pos.addCol(1))
            else if t == Type2.BoxRight then Set(pos, pos.addCol(-1))
            else Set(pos)
          .toSet
        tryPush(nextPos.filter(data.contains) ++ expanded, dir, acc ++ pos)

    tryPush(pos, dir, Set.empty)

  val result2 = moves.foldLeft((startPosition2, data2)) { case ((robotPos, data), moveDirection) =>
    val movedPositions = push2(Set(robotPos), moveDirection, data)
    val movedData      = movedPositions.map(pos => pos.move(moveDirection) -> data(pos)).toMap
    val newData        = data -- movedPositions ++ movedData
    val newRobotPos    = if (movedPositions.nonEmpty) robotPos.move(moveDirection) else robotPos
    println(moveDirection)
    print2(newData)
    (newRobotPos, newData)
  }

  val coordinates2 = result2._2.filter(_._2 == Type2.BoxLeft).keys.map(p => p.row * 100 + p.col).sum
  Predef.println(coordinates2) // 1397393
