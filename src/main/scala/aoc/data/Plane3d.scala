package aoc.data

case class Plane3d[T](start: Point3d[T], normal: Point3d[T])(using numeric: Numeric[T]) {
  import numeric.*

  def contains(point: Point3d[T]): Boolean = {
    val v = point - start
    val d = v dot normal
    d == zero
  }

  def distance(point: Point3d[T]): T = {
    val v = point - start
    val d = v dot normal
    d.abs
  }

  def intersect(line: Line3d[T]): Option[Either[Line3d[T], Point3d[BigDecimal]]] = {
    def toBigDecimal(v: T)          = BigDecimal(v.toString)
    given Conversion[T, BigDecimal] = toBigDecimal
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

  def intersect2(line: Line3d[T]): Option[Either[Line3d[T], Point3d[BigDecimal]]] = {
    def toBigDecimal(v: T) = BigDecimal(v.toString)
    // given Conversion[T, BigDecimal] = toBigDecimal
    val ndotu = normal dot line.direction

    // Check if the line is parallel to the plane
    if (Math.abs(numeric.toDouble(ndotu)) < 1e-6) {
      // The line is parallel to the plane, check if it lies on the plane
      val pdotu = normal dot Point3d(line.start.x - start.x, line.start.y - start.y, line.start.z - start.z)

      if (Math.abs(numeric.toDouble(pdotu)) < 1e-6) {
        // The line lies on the plane
        Some(Right(line.start.mapType(toBigDecimal)))
      } else {
        // The line is parallel but does not lie on the plane
        None
      }
    } else {
      // Calculate the parameter (t) for the line intersection
      val t = -(BigDecimal((normal dot (line.start - this.start)).toString) / BigDecimal(ndotu.toString))

      // Calculate the intersection point
      // val intersectionPoint = line.start + line.direction * numeric.fromDouble(t)

      Some(Left(Line3d(line.start, line.direction))) // or Some(Right(intersectionPoint))
    }
  }
}
