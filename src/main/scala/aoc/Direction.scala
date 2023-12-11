package aoc

enum Direction:
  def left = this match
    case Up    => Left
    case Down  => Right
    case Left  => Down
    case Right => Up
    case North => West
    case South => East
    case East  => North
    case West  => South
  def right = this match
    case Up    => Right
    case Down  => Left
    case Left  => Up
    case Right => Down
    case North => East
    case South => West
    case East  => South
    case West  => North
  case Up, Down, Left, Right, North, South, East, West
end Direction
object Direction:
  // aliases for convenience
  val N = North
  val S = South
  val E = East
  val W = West
  val U = Up
  val D = Down
  val L = Left
  val R = Right
end Direction
