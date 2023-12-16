package aoc.utils

object GridUtils {
  def inPlaceRotateRight[T](grid: Array[Array[T]]): Array[Array[T]] = {
    if (grid.nonEmpty && grid.forall(_.nonEmpty)) {
      val numRows = grid.length
      val numCols = grid(0).length

      for {
        layer <- 0 until numRows / 2
        i     <- layer until numCols - 1 - layer
      } {
        val temp = grid(layer)(i)
        grid(layer)(i) = grid(numRows - 1 - i)(layer)
        grid(numRows - 1 - i)(layer) = grid(numRows - 1 - layer)(numCols - 1 - i)
        grid(numRows - 1 - layer)(numCols - 1 - i) = grid(i)(numCols - 1 - layer)
        grid(i)(numCols - 1 - layer) = temp
      }
    }
    grid
  }

  def inPlaceRotateLeft[T](grid: Array[Array[T]]): Array[Array[T]] = {
    if (grid.nonEmpty && grid.forall(_.nonEmpty)) {
      val numRows = grid.length
      val numCols = grid(0).length

      for {
        layer <- 0 until numRows / 2
        i     <- layer until numCols - 1 - layer
      } {
        val temp = grid(layer)(i)
        grid(layer)(i) = grid(i)(numCols - 1 - layer)
        grid(i)(numCols - 1 - layer) = grid(numRows - 1 - layer)(numCols - 1 - i)
        grid(numRows - 1 - layer)(numCols - 1 - i) = grid(numRows - 1 - i)(layer)
        grid(numRows - 1 - i)(layer) = temp
      }
    }
    grid
  }

  def inPlaceRotate90Degrees[T](m: Array[Array[T]]): Array[Array[T]] =
    inPlaceRotateRight(inPlaceRotateRight(m))

  def prettyPrint[T](grid: Array[Array[T]]): String =
    grid.map(_.mkString).mkString("\n")
}
