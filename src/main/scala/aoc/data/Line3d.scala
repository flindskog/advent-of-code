package aoc.data

case class IntersectionData(
    intersectionPoint: Point3d[BigDecimal],
    distanceFromThis: BigDecimal,
    distanceFromOther: BigDecimal
)

case class Line3d[T](start: Point3d[T], direction: Point3d[T])(using numeric: Numeric[T]) {
  import numeric.*

  def intersection(other: Line3d[T]): Option[IntersectionData] = {
    def toBigDecimal(v: T)          = BigDecimal(v.toString)
    given Conversion[T, BigDecimal] = toBigDecimal
    val epsilon: Double             = 1e-6

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

  def plane(other: Line3d[T]): Option[Plane3d[T]] = {
    val normal = this.direction.cross(other.direction)
    if (normal == Point3d.zero) None else Some(Plane3d(this.start, normal))
  }
}
