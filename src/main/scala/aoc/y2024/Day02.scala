package aoc.y2024

object Day02 extends Aoc2024("input_02.txt"):

  val inputLists = input.map(_.split(" ").map(_.toInt).toList)
  val diff       = (l: List[Int]) => l.zip(l.tail).map(_ - _)

  val diffs = inputLists.map(diff)

  val legalPositive = (d: Int) => d > 0 && d <= 3
  val legalNegative = (d: Int) => d < 0 && d >= -3

  val isLegal = (d: List[Int]) => d.forall(legalPositive) || d.forall(legalNegative)

  val result1 = diffs.count(isLegal)

  println(result1) // 257

  def dampen[T](list: List[T], index: Int): List[T] =
    list.take(index) ++ list.drop(index + 1)

  val dampened = inputLists.map: d =>
    d :: d.indices
      .map: i =>
        dampen(d, i)
      .toList

  val result2 = dampened.count: d =>
    val diffs = d.map(diff)
    diffs.exists(isLegal)

  println(result2) // 328
