package aoc.utils

import scala.annotation.tailrec

object Math {

  /**
   * Calculates the greatest common divisor of two numbers.
   *
   * @param a first number
   * @param b second number
   * @return greatest common divisor
   */
  @tailrec
  def gcd(a: Long, b: Long): Long =
    if (b == 0) a else gcd(b, a % b)

  /**
   * Calculates the least common multiple of two numbers.
   * @param a first number
   * @param b second number
   * @return least common multiple
   */
  def lcm(a: Long, b: Long): Long =
    if (a == 0 || b == 0) 0
    else scala.math.abs(a * b) / gcd(a, b)

  /**
   * Calculates the least common multiple of a list of numbers.
   * @param numbers list of numbers
   * @return least common multiple
   */
  def lcm(numbers: List[Long]): Long =
    numbers.reduce(lcm)
}
