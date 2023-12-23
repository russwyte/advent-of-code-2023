package aoc

case class Point(x: Int, y: Int) extends PointLike[Point]:
  import Direction.*
  def adjacent: Set[Point] = Set(Up, Down, Left, Right).map(move(_))
  def adjacentMap: Map[Direction, Point] =
    Map(
      Direction.North -> move(Direction.North),
      Direction.South -> move(Direction.South),
      Direction.East  -> move(Direction.East),
      Direction.West  -> move(Direction.West),
    )
  def move(d: Direction, distance: Int = 1): Point = d match
    case Up | North   => copy(y = y - distance)
    case Down | South => copy(y = y + distance)
    case Left | West  => copy(x = x - distance)
    case Right | East => copy(x = x + distance)
    case Stop         => this

  def asTuple: (Int, Int)     = (x, y)
  def min(that: Point): Point = Point(x min that.x, y min that.y)
  def max(that: Point): Point = Point(x max that.x, y max that.y)

  def +(that: Point): Point                  = Point(x + that.x, y + that.y)
  def -(that: Point): Point                  = Point(x - that.x, y - that.y)
  def *(n: Int): Point                       = Point(x * n, y * n)
  def <=(that: Point): Boolean               = x <= that.x && y <= that.y
  def offset(that: Point, count: Int): Point = this + (that * count)
  def rotateLeft: Point                      = Point(-y, x)
  def rotateRight: Point                     = Point(y, -x)
  def manhattanDistance(that: Point): Int    = (x - that.x).abs + (y - that.y).abs
  def cross[A](that: Point)(using aNumeric: Numeric[A]): A =
    import scala.math.Numeric.Implicits.infixNumericOps
    aNumeric.fromInt(x) * aNumeric.fromInt(that.y) - aNumeric.fromInt(that.x) * aNumeric.fromInt(y)

end Point
object Point:
  extension [T](grid: IndexedSeq[IndexedSeq[T]]) def apply(p: Point): T = grid(p.y)(p.x)

  extension (grid: IndexedSeq[String]) def apply(p: Point): Char = grid(p.y)(p.x)
