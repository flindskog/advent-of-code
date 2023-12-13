package aoc.utils

object Text {

  /**
   * Compute the Levenstein distance between two strings.
   */
  def levenshteinDistance(line1: String, line2: String): Int = {
    val m = line1.length
    val n = line2.length
    val d = Array.ofDim[Int](m + 1, n + 1)

    for i <- 0 to m do d(i)(0) = i
    for j <- 0 to n do d(0)(j) = j

    for j <- 1 to n do
      for i <- 1 to m do
        if line1(i - 1) == line2(j - 1) then d(i)(j) = d(i - 1)(j - 1)
        else
          d(i)(j) = List(
            d(i - 1)(j) + 1,
            d(i)(j - 1) + 1,
            d(i - 1)(j - 1) + 1
          ).min

    d(m)(n)
  }
}
