package aoc.y2023

import scala.annotation.tailrec

object Day06 extends Aoc2023("input_06.txt"):
  val times          = input.head.dropWhile(_ != ':').tail.trim.split(" +").map(_.toInt)
  val records        = input(1).dropWhile(_ != ':').tail.trim.split(" +").map(_.toInt)
  val timeAndRecords = times.zip(records)

  val result = timeAndRecords
    .map: (time, record) =>
      (0 until time)
        .map: speed =>
          speed * (time - speed)
        .count(_ > record)
    .product

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

  val time2   = times.foldLeft("")((acc, time) => acc + time).toLong
  val record2 = records.foldLeft("")((acc, time) => acc + time).toLong

  @tailrec
  def find(time: Long, speed: Long, nxtFn: Long => Long): Long = {
    val distance = speed * (time - speed)
    if distance > record2 then speed
    else find(time, nxtFn(speed), nxtFn)
  }
  val minSpeed2 = record2 / time2
  val maxSpeed2 = time2

  val first = find(time2, minSpeed2, _ + 1)
  val last  = find(time2, maxSpeed2, _ - 1)

  println(last - first + 1)
