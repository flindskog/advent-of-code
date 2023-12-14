package aoc.utils

import scala.reflect.ClassTag

object ArrayUtils {
  def inPlaceRotateRight[T](array: Array[Array[T]]): Array[Array[T]] = {
    if (array.nonEmpty && array.forall(_.nonEmpty)) {
      val numRows = array.length
      val numCols = array(0).length

      for {
        layer <- 0 until numRows / 2
        i     <- layer until numCols - 1 - layer
      } {
        val temp = array(layer)(i)
        array(layer)(i) = array(numRows - 1 - i)(layer)
        array(numRows - 1 - i)(layer) = array(numRows - 1 - layer)(numCols - 1 - i)
        array(numRows - 1 - layer)(numCols - 1 - i) = array(i)(numCols - 1 - layer)
        array(i)(numCols - 1 - layer) = temp
      }
    }
    array
  }

  def inPlaceRotateLeft[T](array: Array[Array[T]]): Array[Array[T]] = {
    if (array.nonEmpty && array.forall(_.nonEmpty)) {
      val numRows = array.length
      val numCols = array(0).length

      for {
        layer <- 0 until numRows / 2
        i     <- layer until numCols - 1 - layer
      } {
        val temp = array(layer)(i)
        array(layer)(i) = array(i)(numCols - 1 - layer)
        array(i)(numCols - 1 - layer) = array(numRows - 1 - layer)(numCols - 1 - i)
        array(numRows - 1 - layer)(numCols - 1 - i) = array(numRows - 1 - i)(layer)
        array(numRows - 1 - i)(layer) = temp
      }
    }
    array
  }

  def inPlaceRotate90Degrees[T](m: Array[Array[T]]): Array[Array[T]] =
    inPlaceRotateRight(inPlaceRotateRight(m))
}
