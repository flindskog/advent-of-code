package aoc.data

enum Direction {
  case Up, Right, Down, Left

  def inverse: Direction = this match {
    case Up    => Down
    case Right => Left
    case Down  => Up
    case Left  => Right
  }

  def turnLeft: Direction = this match {
    case Up    => Left
    case Right => Up
    case Down  => Right
    case Left  => Down
  }

  def turnRight: Direction = this match {
    case Up    => Right
    case Right => Down
    case Down  => Left
    case Left  => Up
  }
}
