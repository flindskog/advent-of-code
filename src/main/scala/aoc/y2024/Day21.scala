package aoc.y2024

import aoc.data.Pos

import scala.collection.mutable

object Day21 extends Aoc2024("input_21.txt"):
  // format: off
  val numPad: Map[Char, Pos] = Map(
    '7' -> Pos(0, 0), '8' -> Pos(0, 1), '9' -> Pos(0, 2),
    '4' -> Pos(1, 0), '5' -> Pos(1, 1), '6' -> Pos(1, 2),
    '1' -> Pos(2, 0), '2' -> Pos(2, 1), '3' -> Pos(2, 2),
                      '0' -> Pos(3, 1), 'A' -> Pos(3, 2)
  )
  val controlPad: Map[Char, Pos] = Map(
                      '^' -> Pos(0, 1), 'A' -> Pos(0, 2),
    '<' -> Pos(1, 0), 'v' -> Pos(1, 1), '>' -> Pos(1, 2)
  )
  // format: on
  val numPadPositions     = numPad.values.toSet
  val controlPadPositions = controlPad.values.toSet

  def genMoves(pad: Map[Char, Pos], valid: Set[Pos])(from: Char, to: Char): Seq[Char] =
    val fromPos = pad(from)
    val toPos   = pad(to)
    val move    = toPos - fromPos

    // We strive to move horizontal first, then vertical
    val moves = Seq(
      Seq.fill(-move.col)('<'),
      Seq.fill(move.col)('>'),
      Seq.fill(-move.row)('^'),
      Seq.fill(move.row)('v')
    ).flatten

    val reverse =
      // Prefer vertical first if going to the right and it's legal
      valid(fromPos.addRow(move.row)) && move.col > 0 ||
        // If moving left/right first is illegal, we reverse the moves
        !valid(fromPos.addCol(move.col))

    (if reverse then moves.reverse else moves) :+ 'A'

  def numPadMoves(from: Char, to: Char): Seq[Char]     = genMoves(numPad, numPadPositions)(from, to)
  def controlPadMoves(from: Char, to: Char): Seq[Char] = genMoves(controlPad, controlPadPositions)(from, to)

  def passThroughControlPads(numControlPads: Int)(moves: Seq[Char]): Long =
    if numControlPads == 0 then moves.length
    else
      val nextMoves: Seq[Char] = ('A' +: moves)
      val steps                = nextMoves.zip(nextMoves.tail)
      steps.map(controlPadMoves(numControlPads - 1)).sum

  val cache = mutable.Map.empty[(Int, Char, Char), Long]
  def controlPadMoves(numControlPads: Int)(from: Char, to: Char): Long =
    cache.getOrElseUpdate(
      (numControlPads, from, to),
      passThroughControlPads(numControlPads)(controlPadMoves(from, to))
    )

  def putCode(extraControlPads: Int)(code: Seq[Char]): Long =
    passThroughControlPads(extraControlPads)(('A' +: code).zip(code).flatMap(numPadMoves))

  def calculateResult(controlPads: Int): Long =
    input.map(code => putCode(controlPads)(code) * code.init.toInt).sum

  println(calculateResult(2))  // 157892
  println(calculateResult(25)) // 197015606336332
