package aoc

enum Direction:
  def opposite = this match
    case Up    => Down
    case Down  => Up
    case Left  => Right
    case Right => Left
    case North => South
    case South => North
    case East  => West
    case West  => East
    case Stop  => Stop
  def left = this match
    case Up    => Left
    case Down  => Right
    case Left  => Down
    case Right => Up
    case North => West
    case South => East
    case East  => North
    case West  => South
    case Stop  => Stop

  def right = this match
    case Up    => Right
    case Down  => Left
    case Left  => Up
    case Right => Down
    case North => East
    case South => West
    case East  => South
    case West  => North
    case Stop  => Stop

  case Up, Down, Left, Right, North, South, East, West, Stop
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
