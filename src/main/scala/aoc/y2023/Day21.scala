package aoc.y2023

import aoc.data.Pos
import com.microsoft.z3.*

object Day21 extends Aoc2023("input_21.txt"):
  val plot = '.'
  val grid = input.toGrid()

  val start = grid.find('S').get
  grid(start.row)(start.col) = plot

  def walkable(pos: Pos): Boolean =
    grid.isInside(pos) &&
      grid(pos.row)(pos.col) == plot

  def infWalkable(pos: Pos): Boolean = {
    val height    = grid.length
    val width     = grid.head.length
    val newHeight = math.floorMod(pos.row, height)
    val newWidth  = math.floorMod(pos.col, width)
    val newPos    = Pos(newHeight, newWidth)
    grid(newPos.row)(newPos.col) == plot
  }

  def walkLoop(walkable: Pos => Boolean): LazyList[Set[Pos]] = {
    def loop(positions: Set[Pos]): LazyList[Set[Pos]] =
      val newPositions = positions
        .flatMap(_.adjacentNeighbors)
        .filter(walkable)
      newPositions #:: loop(newPositions)

    loop(Set(start))
  }

  // Part 1
  val res = walkLoop(walkable).take(64).last.size
  println(res) // 3689

  // Part 2
  val gridWidth  = grid.head.length
  val iterations = 26501365

  val cycleLength = gridWidth
  val rest        = iterations % cycleLength
  val cycles      = iterations / cycleLength

  import aoc.syntax.z3real.*

  given ctx: Context = new Context()
  val solver         = ctx.mkSolver()
  val Seq(a, b, c)   = mkConstants("a", "b", "c")

  walkLoop(infWalkable).zipWithIndex
    .map((s, idx) => (idx + 1, s.size))
    .filter((idx, _) => idx % gridWidth == rest)
    .take(3)
    .foreach { (iterations, tiles) =>
      solver.add(tiles === a * iterations * iterations + b * iterations + c)
    }

  val status = solver.check()
  status match {
    case Status.SATISFIABLE =>
      println(solver.getModel.eval(a * iterations * iterations + b * iterations + c, false)) // 610158187362102
    case status =>
      println(s"Couldn't solve: $status")
  }
