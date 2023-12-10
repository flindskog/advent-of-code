package aoc.y2022

object Day04 extends Aoc2022("input_04.txt"):
  val data = input
    .map: line =>
      val Array(first, second)    = line.split(",")
      val Array(first1, first2)   = first.split("-").map(_.toInt)
      val Array(second1, second2) = second.split("-").map(_.toInt)
      ((first1, first2), (second1, second2))

  val result = data.filter: data =>
    val ((first1, first2), (second1, second2)) = data
    (first1 <= second1 && second2 <= first2) || (second1 <= first1 && first2 <= second2)

  println(result.size)

  val result2 = data.filter: data =>
    val ((first1, first2), (second1, second2)) = data
    (first1 <= second1 && first1 >= second1) ||
    (first2 >= second1 && first2 <= second2) ||
    (second1 <= first1 && second1 >= first1) ||
    (second2 >= first1 && second2 <= first2)

  println(result2.size)
