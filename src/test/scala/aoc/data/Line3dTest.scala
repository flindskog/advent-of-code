package aoc.data

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Line3dTest extends AnyFlatSpec with Matchers with OptionValues {
  behavior of "intersection"
  it should "find the intersection of two lines" in {
    val line1 = Line3d(Point3d(0, 0, 0), Point3d(1, 1, 1))
    val line2 = Line3d(Point3d(0, 1, 0), Point3d(1, 0, 1))
    line1.intersection(line2).value.intersectionPoint shouldBe Point3d(1, 1, 1)
  }

  it should "not find intersection of skew lines" in {
    val line1 = Line3d(Point3d(0, 0, 0), Point3d(1, 1, 1))
    val line2 = Line3d(Point3d(0, 1, 0), Point3d(1, 0, 0))
    line1.intersection(line2) shouldBe None
  }

  it should "not find intersection of parallel lines" in {
    val line1 = Line3d(Point3d(0, 0, 0), Point3d(1, 1, 1))
    val line2 = Line3d(Point3d(0, 0, 1), Point3d(1, 1, 1))
    line1.intersection(line2) shouldBe None
  }
}
