package aoc

case class Point4(x: Int, y: Int, z: Int, w: Int) extends PointLike[Point4]:
  def *(n: Int): Point4 =
    Point4(x * n, y * n, z * n, w * n)
  def +(that: Point4): Point4 =
    Point4(x + that.x, y + that.y, z + that.z, w + that.w)
  def -(that: Point4): Point4 =
    Point4(x - that.x, y - that.y, z - that.z, w - that.w)
  def manhattanDistance(that: Point4): Int =
    (x - that.x).abs + (y - that.y).abs + (z - that.z).abs + (w - that.w).abs
  def offset(that: Point4, count: Int): Point4 =
    this + (that * count)
  def min(that: Point4): Point4 =
    Point4(x min that.x, y min that.y, z min that.z, w min that.w)
  def max(that: Point4): Point4 =
    Point4(x max that.x, y max that.y, z max that.z, w max that.w)
  def <=(that: Point4): Boolean =
    x <= that.x && y <= that.y && z <= that.z && w <= that.w
end Point4

case class Area4(min: Point4, max: Point4):
  def iterator: Iterator[Point4] =
    for
      x <- (min.x to max.x).iterator
      y <- (min.y to max.y).iterator
      z <- (min.z to max.z).iterator
      w <- (min.w to max.w).iterator
    yield Point4(x, y, z, w)

  def intersect(that: Area4): Option[Area4] =
    val intersectMin = min max that.min
    val intersectMax = max min that.max
    Option.when(intersectMin <= intersectMax)(Area4(intersectMin, intersectMax))

  def size[A](using cNumeric: Numeric[A]): A =
    import scala.math.Numeric.Implicits.infixNumericOps
    val d = max - min + Point4(1, 1, 1, 1)
    cNumeric.fromInt(d.x) * cNumeric.fromInt(d.y) * cNumeric.fromInt(d.z) * cNumeric.fromInt(d.w)
end Area4
