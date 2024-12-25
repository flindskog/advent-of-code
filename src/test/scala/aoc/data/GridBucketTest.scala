package aoc.data

import org.scalatest.flatspec.AnyFlatSpec

class GridBucketTest extends AnyFlatSpec {

  behavior of "GridBucket"

  it should "add and find points" in {
    val bucket =
      new GridBucket(5)(Set(Pos(-6, -5), Pos(-5, -6), Pos(-5, -5), Pos(0, 0), Pos(5, 5), Pos(5, 6), Pos(6, 5)))
    assert(bucket.findWithinDistance(Pos(0, 0), 10) == Seq(Pos(-5, -5), Pos(0, 0), Pos(5, 5)))
  }

}
