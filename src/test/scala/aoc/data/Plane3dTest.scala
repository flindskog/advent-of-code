package aoc.data

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Plane3dTest extends AnyFlatSpec with Matchers with OptionValues {
  it should "create from two intersecting lines" in {
    val line1 = Line3d(Point3d(6, 2, 3), Point3d(5, 2, 1))
    val line2 = Line3d(Point3d(1, 0, 2), Point3d(-1, -1, 1))
    val plane = Plane3d.fromLines(line1, line2)
    plane.value shouldBe Plane3d(Point3d(1, 0, 2), Point3d(3, -6, -3))
    plane.value.intersect(line1.mapType(bi => BigDecimal(bi.toString))) shouldBe Some(Left(line1))
    plane.value.intersect(line2.mapType(bi => BigDecimal(bi.toString))) shouldBe Some(Left(line2))
  }
}
