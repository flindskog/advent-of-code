package aoc.y2024

import scala.annotation.tailrec

object Day17 extends Aoc2024("input_17.txt"):

  case class Registers(a: Long, b: Long, c: Long)

  enum Instruction:
    case Adv, Bxl, Bst, Jnz, Bxc, Out, Bdv, Cdv

  case class Result(registers: Registers, jump: Option[Int] = None, output: Option[Int] = None)

  def comboOperand(op: Int, registers: Registers): Long =
    if op == 4 then registers.a
    else if op == 5 then registers.b
    else if op == 6 then registers.c
    else op

  def op(r: Registers)(instruction: Instruction, operand: Int): Result = instruction match
    case Instruction.Adv =>
      Result(r.copy(a = (r.a / math.pow(2, comboOperand(operand, r))).toLong))
    case Instruction.Bxl =>
      Result(r.copy(b = r.b ^ operand))
    case Instruction.Bst =>
      Result(r.copy(b = comboOperand(operand, r) % 8))
    case Instruction.Jnz =>
      if r.a == 0 then Result(r)
      else Result(r, jump = Some(operand))
    case Instruction.Bxc =>
      Result(r.copy(b = r.b ^ r.c))
    case Instruction.Out =>
      Result(r, output = Some((comboOperand(operand, r) % 8).toInt))
    case Instruction.Bdv =>
      Result(r.copy(b = (r.a / math.pow(2, comboOperand(operand, r))).toLong))
    case Instruction.Cdv =>
      Result(r.copy(c = (r.a / math.pow(2, comboOperand(operand, r))).toLong))

  val (rString, instrStr) = input.splitByEmptyLine match
    case LazyList(r, instr) => r -> instr
    case _                  => sys.error("Invalid input")

  val aPattern     = "Register A: ([0-9]+)".r
  val bPattern     = "Register B: ([0-9]+)".r
  val cPattern     = "Register C: ([0-9]+)".r
  val progrPattern = "Program: (.+)".r
  val registers = rString match
    case LazyList(aPattern(a), bPattern(b), cPattern(c)) =>
      Registers(a.toInt, b.toInt, c.toInt)
    case _ => sys.error("Invalid input, couldn't parse registers")

  val program = instrStr.head match
    case progrPattern(str) => str.split(",").map(_.toInt)
    case _                 => sys.error("Invalid input, couldn't parse program")

  def run(program: Array[Int], registers: Registers): (Registers, List[Int]) =
    @tailrec
    def _loop(pointer: Int, registers: Registers, accOut: Vector[Option[Int]]): (Registers, List[Int]) =
      if pointer >= program.length then (registers, accOut.flatten.toList)
      else
        val instruction                        = Instruction.fromOrdinal(program(pointer))
        val operand                            = program(pointer + 1)
        val Result(newRegisters, jump, output) = op(registers)(instruction, operand)
        val newPointer                         = jump.getOrElse(pointer + 2)
        _loop(newPointer, newRegisters, accOut :+ output)

    _loop(0, registers, Vector.empty)

  val (endRegisters, output) = run(program, registers)

  println(s"output=${output.mkString(",")}") // 7,3,0,5,7,1,4,0,5
  println(s"endRegisters=$endRegisters")
