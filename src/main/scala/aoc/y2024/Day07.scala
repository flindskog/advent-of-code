package aoc.y2024

object Day07 extends Aoc2024("input_07.txt"):
  val lineRegex = """^(\d+): (.*)$""".r

  val data = input.map { case lineRegex(result, numbers) =>
    result.toLong -> numbers.split(" ").map(_.toLong).toList
  }

  enum Operator:
    case Add, Mul, Concat

  def combinations[T](length: Int, choices: List[T]): List[List[T]] =
    if length <= 1 then choices.map(List(_))
    else
      val prev = combinations(length - 1, choices)
      prev.flatMap { list =>
        choices.map(_ :: list)
      }

  def canFindSolution(result: Long, numbers: List[Long], operators: List[Operator]): Boolean = {
    val results = numbers match
      case head :: tail =>
        val combs = combinations(tail.size, operators)
        combs
          .map: operators =>
            tail.zip(operators).foldLeft(head) { case (acc, (number, operator)) =>
              operator match
                case Operator.Add    => acc + number
                case Operator.Mul    => acc * number
                case Operator.Concat => acc.toString.concat(number.toString).toLong
            }
          .toSet
      case Nil =>
        Set.empty[Long]

    results.contains(result)
  }

  def run(operators: List[Operator]) =
    data.filter { case (result, numbers) => canFindSolution(result, numbers, operators) }
      .map(_._1)
      .sum

  println(run(List(Operator.Add, Operator.Mul)))                  // 1985268524462
  println(run(List(Operator.Add, Operator.Mul, Operator.Concat))) // 150077710195188
