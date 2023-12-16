package aoc.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArrayUtilsSpec extends AnyFlatSpec with Matchers {
  it should "rotate right in place" in {
    val array = Array(
      Array(1, 2, 3),
      Array(4, 5, 6),
      Array(7, 8, 9)
    )
    GridUtils.inPlaceRotateRight(array)
    array should be(
      Array(
        Array(7, 4, 1),
        Array(8, 5, 2),
        Array(9, 6, 3)
      )
    )
  }

  it should "rotate left in place" in {
    val array = Array(
      Array(1, 2, 3),
      Array(4, 5, 6),
      Array(7, 8, 9)
    )
    GridUtils.inPlaceRotateLeft(array)
    array should be(
      Array(
        Array(3, 6, 9),
        Array(2, 5, 8),
        Array(1, 4, 7)
      )
    )
  }

  it should "rotate 90 degrees in place" in {
    val array = Array(
      Array(1, 2, 3),
      Array(4, 5, 6),
      Array(7, 8, 9)
    )
    GridUtils.inPlaceRotate90Degrees(array)
    array should be(
      Array(
        Array(9, 8, 7),
        Array(6, 5, 4),
        Array(3, 2, 1)
      )
    )
  }
}
