package aoc.data

case class IntersectionData(
    intersectionPoint: Point3d[BigDecimal],
    distanceFromThis: BigDecimal,
    distanceFromOther: BigDecimal
)

case class Line3d[T](start: Point3d[T], direction: Point3d[T])(using numeric: Numeric[T]) {
  import numeric.*

  private def toBigDecimal(v: T)                 = BigDecimal(v.toString)
  private val epsilon: Double                    = 1e-6
  private def same(x: BigDecimal, y: BigDecimal) = (x - y).abs < epsilon

  def contains(point: Point3d[T]): Boolean =
    given Conversion[T, BigDecimal] = toBigDecimal
    if (direction.x != zero) {
      val t = (point.x - start.x) / direction.x
      if (t < zero) false
      else {
        val y = start.y + direction.y * t
        val z = start.z + direction.z * t
        same(point.y, y) && same(point.z, z)
      }
    } else if (direction.y != zero) {
      val t = (point.y - start.y) / direction.y
      if (t < zero) false
      else {
        val x = start.x + direction.x * t
        val z = start.z + direction.z * t
        same(point.x, x) && same(point.z, z)
      }
    } else if (direction.z != zero) {
      val t = (point.z - start.z) / direction.z
      if (t < zero) false
      else {
        val x = start.x + direction.x * t
        val y = start.y + direction.y * t
        same(point.x, x) && same(point.y, y)
      }
    } else {
      false
    }

  def intersection(other: Line3d[T]): Option[IntersectionData] = {
    given Conversion[T, BigDecimal] = toBigDecimal

    val crossProduct = this.direction.cross(other.direction)

    // Check if lines are parallel (cross product is zero)
    if (crossProduct.isZero(epsilon)) {
      // Lines are parallel, check if they are colinear
      val startDiff = other.start - this.start
      if (startDiff.cross(this.direction).isZero(epsilon)) {
        // Lines are colinear, check for overlapping segments
        val t1 = startDiff.dot(this.direction) / this.direction.dot(this.direction)
        val t2 = t1 + other.direction.dot(this.direction) / this.direction.dot(this.direction)

        if ((t1 >= 0d && t1 <= 1d) || (t2 >= 0d && t2 <= 1d) || (t1 <= 0d && t2 >= 1d) || (t2 <= 0d && t1 >= 1d)) {
          // Overlapping segments, return the intersection point
          // Overlapping segments, calculate the intersection point
          val intersectionPoint = this.start.mapType(toBigDecimal) + this.direction.mapType(toBigDecimal) * t1
          val distanceFromThis  = t1
          val distanceFromOther =
            (intersectionPoint - other.start.mapType(toBigDecimal)).dot(other.direction.mapType(toBigDecimal))
          Some(IntersectionData(intersectionPoint, distanceFromThis, distanceFromOther))
        } else {
          // Lines are colinear but do not overlap
          None
        }
      } else {
        // Lines are parallel but not colinear
        None
      }
    } else {
      // Lines are not parallel, calculate the intersection point
      val startDiff = other.start - this.start
      val t = startDiff.cross(other.direction).dot(crossProduct) /
        this.direction.cross(other.direction).dot(crossProduct)
      val intersectionPoint = this.start.mapType(toBigDecimal) + this.direction.mapType(toBigDecimal) * t

      val distanceFromThis = t
      val distanceFromOther =
        (intersectionPoint - other.start.mapType(toBigDecimal)).dot(other.direction.mapType(toBigDecimal))
      Some(IntersectionData(intersectionPoint, distanceFromThis, distanceFromOther))
    }
  }

  def mapType[T2](f: T => T2)(using Numeric[T2]): Line3d[T2] = Line3d(start.mapType(f), direction.mapType(f))
}

object Line3d {
  def fromTwoPoints[T](p1: Point3d[T], p2: Point3d[T])(using numeric: Numeric[T]): Line3d[T] =
    Line3d(p1, p2 - p1)
}
