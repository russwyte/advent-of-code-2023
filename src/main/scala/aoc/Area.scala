package aoc

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

  def pointsIterator = for
    y <- yRange.iterator
    x <- xRange
  yield Point(x, y)

  def draw(f: Point => Char): String =
    val sb = collection.mutable.StringBuilder()
    for y <- yRange do
      for x <- xRange do sb.addOne(f(Point(x, y)))
      sb.addOne('\n')
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

  def apply(left: Int, right: Int, top: Int, bottom: Int): Area =
    Area(left to right, top to bottom)
end Area
