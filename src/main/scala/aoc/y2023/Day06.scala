package aoc.y2023

import aoc.utils.Input

import scala.annotation.tailrec

trait Day06:
  val data           = Input.read("2023/input_06.txt").toList
  val times          = data(0).dropWhile(_ != ':').tail.trim.split(" +").map(_.toInt)
  val records        = data(1).dropWhile(_ != ':').tail.trim.split(" +").map(_.toInt)
  val timeAndRecords = times.zip(records)

object Day06_1 extends App with Day06:
  val result = timeAndRecords
    .map: (time, record) =>
      (0 until time)
        .map: speed =>
          speed * (time - speed)
        .count(_ > record)
    .product

  println(result)

object Day06_2 extends App with Day06:
  val time   = times.foldLeft("")((acc, time) => acc + time).toLong
  val record = records.foldLeft("")((acc, time) => acc + time).toLong

  @tailrec
  def calcDistance(time: Long, speed: Long, accDistances: Seq[Long]): Seq[Long] = {
    val distance = speed * (time - speed)
    if distance <= record then
      if accDistances.nonEmpty && distance < accDistances.head then accDistances // Declining, we're done
      else calcDistance(time, speed + 1, accDistances)
    else calcDistance(time, speed + 1, distance +: accDistances)
  }

  val minSpeed  = record / time
  val distances = calcDistance(time, minSpeed, Seq.empty)

  println(distances.size)

object Day06_2_2 extends App with Day06:
  val time   = times.foldLeft("")((acc, time) => acc + time).toLong
  val record = records.foldLeft("")((acc, time) => acc + time).toLong

  @tailrec
  def find(time: Long, speed: Long, nxtFn: Long => Long): Long = {
    val distance = speed * (time - speed)
    if distance > record then speed
    else find(time, nxtFn(speed), nxtFn)
  }
  val minSpeed = record / time
  val maxSpeed = time

  val first = find(time, minSpeed, _ + 1)
  val last  = find(time, maxSpeed, _ - 1)

  println(last - first + 1)
