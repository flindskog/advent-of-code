package aoc.y2024

object Day25 extends Aoc2024("input_25.txt"):
  val data = input.splitByEmptyLine.map(_.toGrid(identity))

  case class Key(depths: Array[Int])
  case class Lock(heights: Array[Int])
  type Part = Key | Lock

  val height = 7
  val width  = 5

  def parse(data: Array[Array[Char]]): Part =
    def isKey: Boolean = data(0).forall(_ == '#')
    def countHashesPerColumn(d: Array[Array[Char]]): Array[Int] =
      def countCol(col: Int): Int =
        d.foldLeft(0) { (acc, row) =>
          if row(col) == '#' then acc + 1
          else acc
        }
      d(0).indices.map(countCol).toArray

    if isKey then Key(countHashesPerColumn(data))
    else Lock(countHashesPerColumn(data))

  def fits(key: Key, lock: Lock): Boolean =
    key.depths.zip(lock.heights).forall((k, l) => k + l <= height)

  def mapPart(part: Part): Either[Key, Lock] = part match
    case k: Key  => Left(k)
    case l: Lock => Right(l)

  val (keys, locks) = data.map(parse).partitionMap(mapPart)

  val result = keys.foldLeft(0) { (acc, key) =>
    acc + locks.count(lock => fits(key, lock))
  }

  println(result) // 2586
