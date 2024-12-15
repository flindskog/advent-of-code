package aoc.y2024

import com.microsoft.z3.{Context, Status}

object Day13 extends Aoc2024("input_13.txt"):
  val aRegex     = """^Button A: X\+([0-9]+), Y\+([0-9]+)$""".r
  val bRegex     = """^Button B: X\+([0-9]+), Y\+([0-9]+)$""".r
  val priceRegex = """^Prize: X=([0-9]+), Y=([0-9]+)$""".r

  case class Pos(x: Long, y: Long)
  case class ClawMachine(a: Pos, b: Pos, prize: Pos)

  val machines = input.splitByEmptyLine.map:
    case LazyList(aRegex(xa, ya), bRegex(xb, yb), priceRegex(xp, yp)) =>
      ClawMachine(Pos(xa.toInt, ya.toInt), Pos(xb.toInt, yb.toInt), Pos(xp.toInt, yp.toInt))
    case _ => sys.error("Invalid input")

  def solve(machine: ClawMachine): Option[(Long, Long)] = {
    import aoc.syntax.z3int.*

    given ctx: Context      = new Context()
    val solver              = ctx.mkSolver()
    val Seq(aTimes, bTimes) = mkConstants("aTimes", "bTimes")

    solver.add(aTimes * machine.a.x + bTimes * machine.b.x === machine.prize.x)
    solver.add(aTimes * machine.a.y + bTimes * machine.b.y === machine.prize.y)

    solver.check() match
      case Status.SATISFIABLE =>
        val a = solver.getModel.eval(aTimes, false).toString.toLong
        val b = solver.getModel.eval(bTimes, false).toString.toLong
        Some((a, b))
      case status =>
        None
  }

  def cost(aTimes: Long, bTimes: Long): Long = 3 * aTimes + bTimes

  println(machines.flatMap(solve).map(cost).sum) // 37901

  val newMachines = machines.map(m => m.copy(prize = Pos(10000000000000L + m.prize.x, 10000000000000L + m.prize.y)))

  println(newMachines.flatMap(solve).map(cost).sum) // 77407675412647
