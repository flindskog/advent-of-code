import util.Input

import scala.annotation.tailrec

case class PartNumber(number: Int, row: Int, start: Int, end: Int) {
  def hasSymbolNeighbour(grid: Array[Array[Char]]): Boolean =
    (for {
      col <- (start - 1) max 0 to ((end + 1) min (grid.cols - 1))
      row <- (row - 1) max 0 to ((row + 1) min (grid.rows - 1))
    } yield grid.isSymbol(row, col)).exists(identity)

  def isNeighbor(row: Int, col: Int): Boolean =
    (row - this.row).abs <= 1 && (this.start - col) <= 1 && (col - this.end) <= 1
}

extension (grid: Array[Array[Char]]) {
  def cols = grid(0).length
  def rows = grid.length

  def isSymbol(row: Int, col: Int): Boolean = {
    val char = grid(row)(col)
    !char.isDigit && char != '.'
  }
}

trait Day03 {
  val grid = Input.read("input_03.txt").map(_.toCharArray).to(Array)

  @tailrec
  final def parsePartNumbers(
      col: Int = 0,
      row: Int = 0,
      curNumber: Option[String] = None,
      accNumbers: List[PartNumber] = Nil
  ): List[PartNumber] =
    if (row == grid.rows) accNumbers // done
    else if (col == grid.cols) {     // end of row
      val acc = curNumber match {
        case Some(value) => PartNumber(value.toInt, row, col - value.length, col - 1) :: accNumbers
        case None        => accNumbers
      }
      parsePartNumbers(0, row + 1, None, acc)
    } else if (grid(row)(col).isDigit) { // read digit
      val newNumber = curNumber match {
        case Some(value) => value + grid(row)(col)
        case None        => grid(row)(col).toString
      }
      parsePartNumbers(col + 1, row, Some(newNumber), accNumbers)
    } else { // move on
      val acc = curNumber match {
        case Some(value) => PartNumber(value.toInt, row, col - value.length, col - 1) :: accNumbers
        case None        => accNumbers
      }
      parsePartNumbers(col + 1, row, None, acc)
    }
}

object Day03_1 extends App with Day03 {
  val partNumbers  = parsePartNumbers()
  val validNumbers = partNumbers.filter(_.hasSymbolNeighbour(grid))

  println(validNumbers.map(_.number).sum)
}

object Day03_2 extends App with Day03 {
  val gearPositions =
    for {
      row <- 0 until grid.rows
      col <- 0 until grid.cols
      if grid(row)(col) == '*'
    } yield (row, col)

  val partNumbers = parsePartNumbers()

  val result = gearPositions.map { (row, col) =>
    partNumbers.filter(_.isNeighbor(row, col))
  }.filter(_.size == 2).map(_.map(_.number).product).sum

  println(result)
}
