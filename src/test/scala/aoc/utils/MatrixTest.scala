package aoc.utils

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MatrixTest extends AnyFlatSpec with Matchers {
  it should "rotate right" in {
    val matrix = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9)
    )

    Matrix.rotateRight(matrix) shouldBe List(
      List(7, 4, 1),
      List(8, 5, 2),
      List(9, 6, 3)
    )
  }

  it should "rotate left" in {
    val matrix = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9)
    )

    Matrix.rotateLeft(matrix) shouldBe List(
      List(3, 6, 9),
      List(2, 5, 8),
      List(1, 4, 7)
    )
  }
}
