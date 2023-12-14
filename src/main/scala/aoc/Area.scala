package aoc

import Direction.*

case class Area(xRange: Range, yRange: Range):
  def left   = xRange.min
  def right  = xRange.max
  def top    = yRange.min
  def bottom = yRange.max

  def width  = xRange.size
  def height = yRange.size
  def size   = width * height

  def topLeft     = Point(left, top)
  def topRight    = Point(right, top)
  def bottomLeft  = Point(left, bottom)
  def bottomRight = Point(right, bottom)

  def topBorder    = Line(topLeft, topRight)
  def bottomBorder = Line(bottomLeft, bottomRight)
  def leftBorder   = Line(topLeft, bottomLeft)
  def rightBorder  = Line(topRight, bottomRight)

  def splitHorizontal(n: Int): (Area, Area) =
    val mid = left + n
    (Area(left to mid, yRange), Area(mid + 1 to right, yRange))

  def splitVertical(n: Int): (Area, Area) =
    val mid = top + n
    (Area(xRange, top to mid), Area(xRange, mid + 1 to bottom))

  def contains(p: Point) =
    xRange.contains(p.x) && yRange.contains(p.y)

  def adjacent(p: Point): Set[Point] =
    Set(
      p.move(Direction.Up),
      p.move(Direction.Down),
      p.move(Direction.Left),
      p.move(Direction.Right),
    ).filter(contains)

  def expand(n: Int): Area =
    copy(left - n to right + n, top - n to bottom + n)

  def points = pointsIterator()
  def pointsIterator(from: Direction = North) =
    from match
      case Up | North =>
        for
          y <- yRange.iterator
          x <- xRange
        yield Point(x, y)
      case Down | South =>
        for
          y <- yRange.reverseIterator
          x <- xRange.reverseIterator
        yield Point(x, y)
      case East | Right =>
        for
          x <- xRange.reverseIterator
          y <- yRange
        yield Point(x, y)
      case West | Left =>
        for
          x <- xRange.iterator
          y <- yRange.reverseIterator
        yield Point(x, y)

  def intersect(that: Area): Option[Area] =
    for
      xInterval <- Interval(xRange).intersect(Interval(that.xRange))
      yInterval <- Interval(yRange).intersect(Interval(that.yRange))
    yield Area(xInterval.toRange, yInterval.toRange)

  def draw(f: Point => Char): String =
    val sb = collection.mutable.StringBuilder()
    for y <- yRange do
      for x <- xRange do sb.addOne(f(Point(x, y)))
      if y < yRange.end - 1 then sb.addOne('\n')
    sb.result()
end Area

object Area:
  def apply(grid: IndexedSeq[IndexedSeq[_]]): Area =
    Area(
      xRange = grid.headOption.fold(0 to 0)(_.indices),
      yRange = grid.indices,
    )

  def apply(grid: IndexedSeq[String])(using wrap: String => collection.immutable.WrappedString): Area =
    apply(grid.map(wrap))

  def bounding[T](points: Map[Point, T]): Area =
    val xs: Iterable[Int] = points.keys.map(_.x)
    val ys: Iterable[Int] = points.keys.map(_.y)
    Area(
      xRange = xs.min to xs.max,
      yRange = ys.min to ys.max,
    )

  def bounding(points: Set[Point]): Area =
    val xs: Set[Int] = points.map(_.x)
    val ys: Set[Int] = points.map(_.y)
    Area(
      xRange = xs.min to xs.max,
      yRange = ys.min to ys.max,
    )

  def bounding(a: Point, b: Point): Area =
    Area(
      xRange = (a.x min b.x) to (a.x max b.x),
      yRange = (a.y min b.y) to (a.y max b.y),
    )

  def apply(left: Int, right: Int, top: Int, bottom: Int): Area =
    Area(left to right, top to bottom)
end Area
