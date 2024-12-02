package aoc.y2023

import aoc.data.{Line3d, Point3d}
import com.microsoft.z3.*

object Day24 extends Aoc2023("input_24.txt"):
  val storms = input.map { line =>
    val Array(p1, p2)     = line.split(" @ ")
    val Array(x1, y1, z1) = p1.split(",").map(_.trim)
    val Array(x2, y2, z2) = p2.split(",").map(_.trim)
    Line3d[BigInt](Point3d[BigInt](x1.toLong, y1.toLong, z1.toLong), Point3d[BigInt](x2.toLong, y2.toLong, z2.toLong))
  }

  val storms2d = storms.map { line =>
    line.copy(start = line.start.withZ(0), direction = line.direction.withZ(0))
  }

  val intersections = storms2d
    .combinations(2)
    .flatMap { case Seq(l1, l2) =>
      l1.intersection(l2).map((l1, l2, _))
    }
    .toList

  val startAt = 200000000000000L
  val endAt   = 400000000000000L

  val valid = intersections.filter { (_, _, intsec) =>
    intsec.intersectionPoint.x >= startAt && intsec.intersectionPoint.x <= endAt &&
    intsec.intersectionPoint.y >= startAt && intsec.intersectionPoint.y <= endAt &&
    intsec.distanceFromThis >= 0 &&
    intsec.distanceFromOther >= 0
  }.toSet

  println(valid.size) // 20847

  // part 2
  {
    import aoc.syntax.z3real.*

    given ctx: Context              = new Context()
    val solver                      = ctx.mkSolver()
    val Seq(x0, y0, z0, dx, dy, dz) = mkConstants("x0", "y0", "z0", "dx", "dy", "dz")

    storms.take(3).zipWithIndex.foreach { (lineBigInt, idx) =>
      val line = lineBigInt.mapType(_.toLong)
      val t    = mkConstant(s"t$idx")

      solver.add(x0 + t * dx === line.start.x + t * line.direction.x)
      solver.add(y0 + t * dy === line.start.y + t * line.direction.y)
      solver.add(z0 + t * dz === line.start.z + t * line.direction.z)
      solver.add(t >= 0) // Not really necessary, just testing the dsl
    }

    solver.check() match {
      case Status.SATISFIABLE =>
        println(solver.getModel.eval(x0 + y0 + z0, false)) // 908621716620524
      case status =>
        println(s"Couldn't solve: $status")
    }
  }
