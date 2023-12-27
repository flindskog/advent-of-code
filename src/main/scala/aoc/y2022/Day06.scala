package aoc.y2022

import scala.annotation.tailrec

object Day06 extends Aoc2022("input_06.txt"):
  val data = input.head

  @tailrec
  def indexAfterFirstMarker(data: String, markerLength: Int, start: Int = 0): Int = {
    val isMarker = data.take(markerLength).toSet.size == markerLength
    if isMarker then start + markerLength
    else indexAfterFirstMarker(data.tail, markerLength, start + 1)
  }

  println(indexAfterFirstMarker(data, 4))  // 1578
  println(indexAfterFirstMarker(data, 14)) // 2178
