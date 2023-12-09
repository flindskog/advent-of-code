package aoc

import aoc.utils.Input

trait Aoc(fileName: String) extends App:
  // it will show the execution time before exiting
  util.Properties.setProp("scala.time", "on")

  val input = Input.read(fileName)
