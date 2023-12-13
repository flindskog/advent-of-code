package aoc.y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day12Spec extends AnyFlatSpec with Matchers {

  behavior of "count"

  it should "count" in {
    Day12.count("??".toSeq, List(2)) shouldBe 1
    Day12.count("?".toSeq, List(1)) shouldBe 1
    Day12.count("??".toSeq, List(1)) shouldBe 2

    Day12.count("??.??.?##".toSeq, List(1, 1, 3)) shouldBe 4

    Day12.count("???.###".toSeq, List(1, 1, 3)) shouldBe 1
    Day12.count(".??..??...?##.".toSeq, List(1, 1, 3)) shouldBe 4
    Day12.count("?#?#?#?#?#?#?#?".toSeq, List(1, 3, 1, 6)) shouldBe 1
    Day12.count("????.#...#...".toSeq, List(4, 1, 1)) shouldBe 1
    Day12.count("????.######..#####.".toSeq, List(1, 6, 5)) shouldBe 4
    Day12.count("?###????????".toSeq, List(3, 2, 1)) shouldBe 10
  }
}
