package aoc.y2024

import scala.annotation.tailrec

object Day09 extends Aoc2024("input_09.txt"):
  val diskMap = input.head.toVector.map(_.asDigit)
  val expanded = diskMap.zipWithIndex.flatMap: (value, index) =>
    if index % 2 == 0 then Vector.fill(value)(index / 2)
    else Vector.fill(value)(-1)

  def compact(initial: Vector[Int]): Vector[Int] = {
    @tailrec
    def _loop(left: Int, right: Int, acc: Vector[Int]): Vector[Int] =
      if left <= right then
        if initial(left) != -1 then _loop(left + 1, right, acc :+ initial(left))
        else if initial(right) != -1 then _loop(left + 1, right - 1, acc :+ initial(right))
        else _loop(left, right - 1, acc)
      else acc

    _loop(0, initial.length - 1, Vector.empty)
  }

  def checksum(fs: Vector[Int]): Long =
    fs.zipWithIndex
      .map: (value, index) =>
        if value != -1 then index * value.toLong
        else 0
      .sum

  val result = checksum(compact(expanded))
  println(result) // 6337367222422

  enum Space:
    case Free(size: Int)
    case File(id: Int, size: Int)

  val mapped = diskMap.zipWithIndex.map: (value, index) =>
    if index % 2 == 0 then Space.File(index / 2, value)
    else Space.Free(value)

  val files = mapped.collect { case f: Space.File => f }.reverse

  def compactWholeFiles(initial: Vector[Space]): Vector[Space] = {
    @tailrec
    def _loop(remainingFiles: Vector[Space.File], acc: Vector[Space]): Vector[Space] =
      if remainingFiles.nonEmpty then
        val file = remainingFiles.head
        val (first, rest) = acc.span {
          case Space.Free(free) if free >= file.size => false
          case _                                     => true
        }
        if rest.isEmpty || first.contains(file) then _loop(remainingFiles.tail, acc)
        else // Found free space
          rest match
            case Space.Free(free) +: rest =>
              val tailRemovedFile = rest.map:
                case f: Space.File if f.id == file.id => Space.Free(f.size)
                case other                            => other

              val tail =
                if file.size == free then tailRemovedFile
                else Space.Free(free - file.size) +: tailRemovedFile
              _loop(remainingFiles.tail, (first :+ file) ++ tail)
            case _ => _loop(remainingFiles.tail, acc)
      else acc

    _loop(files, mapped)
  }

  val fileCompacted = compactWholeFiles(mapped)
  val asExpandedDiskMap = fileCompacted.flatMap {
    case Space.Free(size)     => Vector.fill(size)(-1)
    case Space.File(id, size) => Vector.fill(size)(id)
  }

  println(checksum(asExpandedDiskMap)) // 6361380647183
