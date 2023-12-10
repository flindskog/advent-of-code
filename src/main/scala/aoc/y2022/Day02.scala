package aoc.y2022

object Day02 extends Aoc2022("input_02.txt"):

  enum RockPaperScissors:
    case Rock, Paper, Scissors

  def toTockPaperScissors(input: String): RockPaperScissors =
    input match
      case "A" | "X" => RockPaperScissors.Rock
      case "B" | "Y" => RockPaperScissors.Paper
      case "C" | "Z" => RockPaperScissors.Scissors

  def score(opponent: RockPaperScissors, me: RockPaperScissors): Int =
    val roundScore = (opponent, me) match
      case (RockPaperScissors.Rock, RockPaperScissors.Paper) | (RockPaperScissors.Paper, RockPaperScissors.Scissors) |
          (RockPaperScissors.Scissors, RockPaperScissors.Rock) =>
        6

      case (o, m) if o == m => 3
      case _                => 0
    val shapeScore = me match
      case RockPaperScissors.Rock     => 1
      case RockPaperScissors.Paper    => 2
      case RockPaperScissors.Scissors => 3
    roundScore + shapeScore

  val result = input
    .map: line =>
      val Array(opponent, me) = line.split(" ")
      toTockPaperScissors(opponent) -> toTockPaperScissors(me)
    .map((score _).tupled)
  println(result.sum)

  val games2 = input
    .map: line =>
      val Array(o, m) = line.split(" ")
      val opponent    = toTockPaperScissors(o)
      val me = m match
        case "X" => // lose
          opponent match
            case RockPaperScissors.Rock     => RockPaperScissors.Scissors
            case RockPaperScissors.Paper    => RockPaperScissors.Rock
            case RockPaperScissors.Scissors => RockPaperScissors.Paper
        case "Y" => // draw
          opponent
        case "Z" => // win
          opponent match
            case RockPaperScissors.Rock     => RockPaperScissors.Paper
            case RockPaperScissors.Paper    => RockPaperScissors.Scissors
            case RockPaperScissors.Scissors => RockPaperScissors.Rock
      opponent -> me
    .map((score _).tupled)

  println(games2.sum)
