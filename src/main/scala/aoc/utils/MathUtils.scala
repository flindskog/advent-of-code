package aoc.utils

import scala.annotation.tailrec

object MathUtils:

  /**
   * Calculates the greatest common divisor of two numbers.
   *
   * @param a first number
   * @param b second number
   * @return greatest common divisor
   */
  @tailrec
  def gcd[T](a: T, b: T)(using integral: Integral[T]): T = {
    import integral.*
    if b == 0 then a else gcd(b, a % b)
  }

  def gcd[T: Integral](numbers: T*): T =
    numbers.reduce(gcd)

  /**
   * Calculates the least common multiple of two numbers.
   * @param a first number
   * @param b second number
   * @return least common multiple
   */
  def lcm[T](a: T, b: T)(using integral: Integral[T]): T = {
    import integral.*
    if a == zero || b == zero then zero
    else {
      val product = a * b
      if product < zero then -product / gcd(a, b) else product / gcd(a, b)
    }
  }

  /**
   * Calculates the least common multiple of a list of numbers.
   * @param numbers list of numbers
   * @return least common multiple
   */
  def lcm[T: Integral](numbers: T*): T =
    numbers.reduce(lcm)
