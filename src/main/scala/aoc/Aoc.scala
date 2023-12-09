package aoc

import aoc.utils.Input

trait Aoc(fileName: String) extends App:
  // Shows the execution time
  util.Properties.setProp("scala.time", "on")

  val input = Input.read(fileName)

  extension (input: LazyList[String])
    def splitByEmptyLine: LazyList[List[String]] =
      input
        .foldLeft(LazyList(List.empty[String])) {
          case (acc, "")   => List.empty[String] #:: acc
          case (acc, line) => (line :: acc.head) #:: acc.tail
        }
        .map(_.reverse)
        .reverse
        .filter(_.nonEmpty)
