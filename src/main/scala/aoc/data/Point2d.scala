package aoc.data

import scala.annotation.targetName

case class Point2d[T](x: T, y: T)(using numeric: Numeric[T]) {
  import numeric.*

  @targetName("addPoint")
  def +(other: Point2d[T]): Point2d[T] = Point2d(x + other.x, y + other.y)

  @targetName("subtractPoint")
  def -(other: Point2d[T]): Point2d[T] = Point2d(x - other.x, y - other.y)

  @targetName("multiplyPoint")
  def *(factor: T): Point2d[T] = Point2d(x * factor, y * factor)

  @targetName("dividePoint")
  def /(factor: Double): Point2d[Double] = Point2d(x.toDouble / factor, y.toDouble / factor)

  def manhattanDistance(other: Point2d[T]): T = (x - other.x).abs + (y - other.y).abs

  def cross(other: Point2d[T]): T = x * other.y - y * other.x

  def dot(other: Point2d[T]): T = x * other.x + y * other.y

  def withX(newX: T): Point2d[T] = Point2d(newX, y)

  def withY(newY: T): Point2d[T] = Point2d(x, newY)
}

object Point2d {
  def zero[T](using numeric: Numeric[T]): Point2d[T] = Point2d(numeric.zero, numeric.zero)
}
