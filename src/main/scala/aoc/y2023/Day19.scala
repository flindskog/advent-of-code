package aoc.y2023

object Day19 extends Aoc2023("input_19.txt"):
  enum Outcome:
    case Accepted, Rejected

  enum Operator:
    case LessThan, GreaterThan

  case class Range(start: Int, end: Int) {
    val size: Long = end - start + 1
  }
  sealed trait Rule
  case class ApplyFunction(category: String, operator: Operator, limit: Int, result: Either[String, Outcome])
      extends Rule
  case class Fixed(outcome: Either[String, Outcome]) extends Rule

  case class Workflow(id: String, applyRules: PartialApply)
  type Ratings      = Map[String, Int]
  type Workflows    = Map[String, Workflow]
  type PartialApply = PartialFunction[(Ratings, Workflows), Outcome]

  val List(workflowData, ratingData) = input.splitByEmptyLine.toList
  val workflowRegex                  = """^(\w+)\{(.*)}$""".r
  val ruleRegex                      = """(\w)([<>])(\d+):(\w+)""".r
  val ratingsRegex                   = """(\w=\d+)""".r

  val ratings = ratingData.map { line =>
    ratingsRegex
      .findAllIn(line)
      .map { case ratingsRegex(group) =>
        val Array(key, value) = group.split('=')
        (key, value.toInt)
      }
      .toMap
  }

  val workflows = workflowData.map { case workflowRegex(id, rules) =>
    val ruleFunctions: List[PartialApply] = rules
      .split(",")
      .map {
        case ruleRegex(id, op, value, res) =>
          val valueInt = value.toInt
          val operator: Int => Boolean = op match {
            case "<" => (a: Int) => a < valueInt
            case ">" => (a: Int) => a > valueInt
          }

          val outcome: PartialApply = res match {
            case "A" => _ => Outcome.Accepted
            case "R" => _ => Outcome.Rejected
            case id  => (rts, wfs) => wfs(id).applyRules(rts, wfs)
          }
          val resultFn: PartialApply = {
            case (rts, wfs) if operator(rts(id)) => outcome(rts, wfs)
          }
          resultFn
        case "A" => _ => Outcome.Accepted
        case "R" => _ => Outcome.Rejected
        case id  => (rts, wfs) => wfs(id).applyRules(rts, wfs)
      }
      .toList

    val sumFn = ruleFunctions.reduce(_ orElse _)

    (id, Workflow(id, sumFn))
  }.toMap

  val outcomes = ratings.map(rating => (rating, workflows("in").applyRules(rating, workflows)))

  val res = outcomes.filter { case (_, outcome) => outcome == Outcome.Accepted }.map { case (rating, _) =>
    rating.values.sum
  }.sum

  println(res) // 406934

  // part 2
  val workflowRules = workflowData.map { case workflowRegex(id, rules) =>
    val theRules: List[Rule] = rules
      .split(",")
      .map {
        case ruleRegex(id, op, value, res) =>
          val valueInt = value.toInt
          val operator = op match {
            case "<" => Operator.LessThan
            case ">" => Operator.GreaterThan
          }

          val outcome: Either[String, Outcome] = res match {
            case "A" => Right(Outcome.Accepted)
            case "R" => Right(Outcome.Rejected)
            case id  => Left(id)
          }

          ApplyFunction(id, operator, valueInt, outcome)
        case "A" => Fixed(Right(Outcome.Accepted))
        case "R" => Fixed(Right(Outcome.Rejected))
        case id  => Fixed(Left(id))
      }
      .toList

    (id, theRules)
  }.toMap

  val startRange = Range(1, 4000)
  val start = Map(
    "x" -> startRange,
    "m" -> startRange,
    "a" -> startRange,
    "s" -> startRange
  )

  def applyRules(ranges: Map[String, Range], currentRules: List[Rule]): List[(Map[String, Range], Outcome)] =
    currentRules match
      case ApplyFunction(id, operator, limit, result) :: tail =>
        val range = ranges(id)
        val (matchRange, noMatchRange) = operator match
          case Operator.LessThan    => range.copy(end = limit - 1)   -> range.copy(start = limit)
          case Operator.GreaterThan => range.copy(start = limit + 1) -> range.copy(end = limit)

        val matchedResult = result match
          case Left(next) =>
            val newRanges = ranges + (id -> matchRange)
            applyRules(newRanges, workflowRules(next))
          case Right(outcome) =>
            val newRanges = ranges + (id -> matchRange)
            List((newRanges, outcome))

        val noMatchResult = applyRules(ranges + (id -> noMatchRange), tail)

        matchedResult ++ noMatchResult
      case Fixed(outcome) :: _ =>
        outcome match
          case Right(outcome) =>
            List((ranges, outcome))
          case Left(next) =>
            applyRules(ranges, workflowRules(next))
      case Nil =>
        sys.error("We should not be here")

  val result2 =
    applyRules(start, workflowRules("in")).filter(_._2 == Outcome.Accepted).map(_._1.values.map(_.size).product).sum

  println(result2)
