package aoc.data

import scala.annotation.targetName

case class Point3d[T](x: T, y: T, z: T)(using numeric: Numeric[T]) {
  import numeric._

  @targetName("addPoint")
  def +(other: Point3d[T]): Point3d[T] = Point3d(x + other.x, y + other.y, z + other.z)

  @targetName("subtractPoint")
  def -(other: Point3d[T]): Point3d[T] = Point3d(x - other.x, y - other.y, z - other.z)

  @targetName("multiplyPoint")
  def *(factor: T): Point3d[T] = Point3d(x * factor, y * factor, z * factor)

  @targetName("dividePoint")
  def /(factor: Double): Point3d[Double] =
    Point3d(x.toDouble / factor, y.toDouble / factor, z.toDouble / factor)

  def cross(other: Point3d[T]): Point3d[T] = Point3d(
    y * other.z - z * other.y,
    z * other.x - x * other.z,
    x * other.y - y * other.x
  )

  def dot(other: Point3d[T]): T = x * other.x + y * other.y + z * other.z

  def isZero(epsilon: Double): Boolean = {
    given Conversion[T, Double] = numeric.toDouble
    math.abs(x) < epsilon &&
    math.abs(y) < epsilon &&
    math.abs(z) < epsilon
  }

  def manhattanDistance(other: Point3d[T]): T = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs

  def mapType[T2](f: T => T2)(using Numeric[T2]): Point3d[T2] = Point3d(f(x), f(y), f(z))

  def withX(newX: T): Point3d[T] = Point3d(newX, y, z)

  def withY(newY: T): Point3d[T] = Point3d(x, newY, z)

  def withZ(newZ: T): Point3d[T] = Point3d(x, y, newZ)

  def xyProjection: Point2d[T] = Point2d(x, y)
}

object Point3d {
  def zero[T](using numeric: Numeric[T]): Point3d[T] = Point3d(numeric.zero, numeric.zero, numeric.zero)
}
