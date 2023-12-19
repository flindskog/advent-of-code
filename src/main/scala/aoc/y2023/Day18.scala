package aoc.y2023

import aoc.data.{Direction, Pos}
import aoc.utils.Polygon

object Day18 extends Aoc2023("input_18.txt"):
  def parseDirection(str: String): Direction = str match
    case "R" => Direction.Right
    case "L" => Direction.Left
    case "U" => Direction.Up
    case "D" => Direction.Down

  case class Instruction(direction: Direction, length: Int)

  private def parseInstructionFromColor(str: String): Instruction = {
    val direction = str.last match
      case '0' => Direction.Right
      case '1' => Direction.Down
      case '2' => Direction.Left
      case '3' => Direction.Up
    val length = Integer.parseInt(str.init, 16)
    Instruction(direction, length)
  }

  val (part1Instructions, part2Instructions) = input.map { line =>
    val regex                           = """(\w) (\d+) \(#([0-9a-f]{6})\)""".r
    val regex(direction, length, color) = line
    (Instruction(parseDirection(direction), length.toInt), parseInstructionFromColor(color))
  }.unzip

  def calculateArea(instructions: Iterable[Instruction]): Long = {
    def toPositions = instructions.scanLeft(Pos.origo) { (pos, instruction) =>
      pos.move(instruction.direction, instruction.length)
    }

    val positions = toPositions
    val innerArea = Polygon.areaOf(positions.toSeq)
    val boundary  = instructions.map(_.length).sum / 2 + 1
    innerArea + boundary
  }

  println(calculateArea(part1Instructions)) // 76387
  println(calculateArea(part2Instructions)) // 250022188522074
