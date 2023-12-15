package aoc.y2023

import aoc.utils.ArrayUtils

import scala.annotation.tailrec
import scala.collection.immutable.VectorMap

object Day15 extends Aoc2023("input_15.txt"):
  val data = input.map(line => line.split(",").toSeq).head

  @tailrec
  def hash(str: String, acc: Int = 0): Int =
    if str.isEmpty then acc
    else hash(str.tail, (acc + str.head.toInt) * 17 % 256)

  val res = data.map(hash(_))
  println(res.sum)

  case class Box(number: Int, lenses: VectorMap[String, Int])
  val boxes = Map[Int, Box]().withDefault(i => Box(i, VectorMap()))

  val fullBoxes = data.foldLeft(boxes) { (boxes, str) =>
    val replaceRegex = """^(\w+)=(\d+)$""".r
    val removeRegex  = """^(\w+)-$""".r

    str match
      case replaceRegex(lens, fl) =>
        val focalLength = fl.toInt
        val boxNo       = hash(lens)
        boxes.updated(boxNo, boxes(boxNo).copy(lenses = boxes(boxNo).lenses.updated(lens, focalLength)))
      case removeRegex(lens) =>
        val boxNo = hash(lens)
        boxes.updated(boxNo, boxes(boxNo).copy(lenses = boxes(boxNo).lenses - lens))
  }

  val res2 = fullBoxes.values.map { box =>
    box.lenses.zipWithIndex.map { case ((_, focalLength), i) =>
      (box.number + 1) * (i + 1) * focalLength
    }.sum
  }.sum

  println(res2)
