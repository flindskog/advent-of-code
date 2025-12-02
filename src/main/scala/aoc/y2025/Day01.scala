package aoc.y2025

object Day01 extends Aoc2025("input_01.txt"):

  enum Direction:
    case Left, Right

  val start = 50
  val max   = 99

  case class Turn(direction: Direction, ticks: Int) {
    private def normalize(position: Int): Int =
      if position >= 0 && position <= max then position
      else if position < 0 then normalize(position + (max + 1))
      else normalize(position - (max + 1))

    def turn(start: Int): Int = {
      val pos = direction match
        case Direction.Left  => start - ticks
        case Direction.Right => start + ticks

      normalize(pos)
    }
  }

  val turns = input.map {
    case s"L$ticks" => Turn(Direction.Left, ticks.toInt)
    case s"R$ticks" => Turn(Direction.Right, ticks.toInt)
    case _          => throw IllegalArgumentException("Invalid input")
  }

  val positions = turns.scanLeft(start) { case (current, turn) =>
    turn.turn(current)
  }

  val result1 = positions.count(_ % (max + 1) == 0)
  println(result1)

  def count0(start: Int, turn: Turn): Int = {
    val fullRotations = turn.ticks / (max + 1)
    val remainder     = turn.ticks % (max + 1)
    val passesZero = turn.direction match {
      case Direction.Left  => remainder >= start && start > 0
      case Direction.Right => remainder + start > max
    }
    fullRotations + (if passesZero then 1 else 0)
  }

  val result2 = positions
    .zip(turns)
    .map { case (position, turn) =>
      count0(position, turn)
    }
    .sum
  println(result2)
