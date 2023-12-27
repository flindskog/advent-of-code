package aoc.data

import aoc.data.Plane3d.toBigDecimal

case class Plane3d[T](start: Point3d[T], normal: Point3d[T])(using numeric: Numeric[T]) {
  import numeric.*

  def contains(point: Point3d[T]): Boolean = {
    val v = point - start
    val d = v dot normal
    math.abs(d.toDouble) < 1e-6
  }

  def distance(point: Point3d[T]): BigDecimal = {
    val v = start - point
    val d = v dot normal
    toBigDecimal(d.abs) / math.sqrt((normal.x * normal.x + normal.y * normal.y + normal.z * normal.z).toDouble)
  }

  def intersect(line: Line3d[T]): Option[Either[Line3d[T], Point3d[BigDecimal]]] = {
    given Conversion[T, BigDecimal] = t => toBigDecimal(t)
    val d                           = line.direction dot normal
    if d == zero then {
      if contains(line.start) then Some(Left(line))
      else None
    } else {
      val t = ((start - line.start) dot normal) / d
      val p = line.start.mapType(toBigDecimal) + line.direction.mapType(toBigDecimal) * t
      Some(Right(p))
    }
  }

  def mapType[T2](f: T => T2)(using Numeric[T2]): Plane3d[T2] = Plane3d(start.mapType(f), normal.mapType(f))
}

object Plane3d {
  def toBigDecimal[T] = (v: T) => BigDecimal(v.toString)

  def fromLines[T](line1: Line3d[T], line2: Line3d[T])(using numeric: Numeric[T]): Option[Plane3d[BigDecimal]] = {
    val normal = line1.direction.cross(line2.direction).mapType(toBigDecimal)
    if normal == Point3d.zero then {
      Some(fromPoints(line1.start, line1.start + line1.direction, line2.start).mapType(toBigDecimal))
    } else {
      line1.intersection(line2).map(intersection => Plane3d(intersection.intersectionPoint, normal))
    }
  }

  def fromPoints[T](p1: Point3d[T], p2: Point3d[T], p3: Point3d[T])(using numeric: Numeric[T]): Plane3d[T] = {
    val v1     = p2 - p1
    val v2     = p3 - p1
    val normal = v1 cross v2
    Plane3d(p1, normal)
  }
}
