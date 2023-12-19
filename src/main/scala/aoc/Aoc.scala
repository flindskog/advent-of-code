package aoc

import aoc.syntax.{AllSyntax, OptionSyntax}
import aoc.utils.Input

import scala.reflect.ClassTag

trait Aoc(fileName: String) extends AllSyntax with App:
  // Shows the execution time
  util.Properties.setProp("scala.time", "on")

  val input = Input.read(fileName)

  extension (input: LazyList[String]) {
    def splitByEmptyLine: LazyList[List[String]] =
      input
        .foldLeft(LazyList(List.empty[String])) {
          case (acc, "")   => List.empty[String] #:: acc
          case (acc, line) => (line :: acc.head) #:: acc.tail
        }
        .map(_.reverse)
        .reverse
        .filter(_.nonEmpty)

    def toGrid[T: ClassTag](mapFn: Char => T = identity): Array[Array[T]] =
      input.map(_.toCharArray.map(mapFn)).toArray
  }
