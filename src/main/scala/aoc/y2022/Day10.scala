package aoc.y2022

import aoc.data.{Direction, Pos}

import scala.annotation.tailrec
import scala.util.Try

object Day10 extends Aoc2022("input_10.txt"):
  enum Instruction:
    case NoOp
    case AddX(value: Int)

  val instructions = input.flatMap {
    case "noop"     => List(Instruction.NoOp)
    case s"addx $i" => List(Instruction.NoOp, Instruction.AddX(i.toInt))
  }

  def process(instructions: List[Instruction]): Vector[Int] = {
    @tailrec
    def loop(instructions: List[Instruction], acc: Vector[Int], index: Int, x: Int): Vector[Int] = {
      val newAcc = if (index + 20) % 40 == 0 then acc :+ x * index else acc
      instructions match
        case Nil                             => newAcc
        case Instruction.NoOp :: tail        => loop(tail, newAcc, index + 1, x)
        case Instruction.AddX(value) :: tail => loop(tail, newAcc, index + 1, x + value)
    }

    loop(instructions, Vector.empty, 0, 1)
  }

  val result = process(instructions.toList).sum
  println(result) // 15120

  def print(instructions: List[Instruction]): Vector[Char] = {
    @tailrec
    def loop(instructions: List[Instruction], acc: Vector[Char], index: Int, x: Int): Vector[Char] = {
      val printChar = if x - 1 to x + 1 contains (index % 40) then '#' else '.'
      instructions match
        case Nil                             => acc
        case Instruction.NoOp :: tail        => loop(tail, acc :+ printChar, index + 1, x)
        case Instruction.AddX(value) :: tail => loop(tail, acc :+ printChar, index + 1, x + value)
    }

    loop(instructions, Vector.empty, 0, 1)
  }

  val result2 = print(instructions.toList).grouped(40).map(_.mkString)

  println(result2.mkString("\n"))
