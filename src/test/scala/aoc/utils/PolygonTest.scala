package aoc.utils

import aoc.data.Pos
import org.scalatest.flatspec.AnyFlatSpec

class PolygonTest extends AnyFlatSpec {
  behavior of "areaOf"

  it should "calculate the area of a triangle" in {
    val triangle = Polygon.areaOf(Seq(Pos(0, 0), Pos(0, 2), Pos(2, 0)))
    assert(triangle == 2)
  }

  it should "calculate the area of a square" in {
    val square = Polygon.areaOf(Seq(Pos(0, 0), Pos(0, 1), Pos(1, 1), Pos(1, 0)))
    assert(square == 1)
  }

  it should "calculate the area of a pentagon" in {
    val pentagon = Polygon.areaOf(Seq(Pos(0, 0), Pos(0, 2), Pos(2, 2), Pos(2, 0), Pos(1, 1)))
    assert(pentagon == 3)
  }

  it should "calculate the area of a complex fence" in {
    val fence = Polygon.areaOf(Seq(Pos(3, 4), Pos(5, 11), Pos(12, 8), Pos(9, 5), Pos(5, 6)))
    assert(fence == 30)
  }
}
