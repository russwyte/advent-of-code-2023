package aoc

case class Point(x: Int, y: Int):
  import Direction.*
  def move(d: Direction, distance: Int = 1): Point = d match
    case Up | North   => copy(y = y - distance)
    case Down | South => copy(y = y + distance)
    case Left | West  => copy(x = x - distance)
    case Right | East => copy(x = x + distance)

  def asTuple: (Int, Int) = (x, y)

  def +(that: Point): Point               = Point(this.x + that.x, this.y + that.y)
  def -(that: Point): Point               = Point(this.x - that.x, this.y - that.y)
  def rotateLeft: Point                   = Point(-y, x)
  def rotateRight: Point                  = Point(y, -x)
  def manhattanDistance(that: Point): Int = (this.x - that.x).abs + (this.y - that.y).abs
end Point
object Point:
  extension [T](grid: IndexedSeq[IndexedSeq[T]]) def apply(p: Point): T = grid(p.y)(p.x)

  extension (grid: IndexedSeq[String]) def apply(p: Point): Char = grid(p.y)(p.x)
