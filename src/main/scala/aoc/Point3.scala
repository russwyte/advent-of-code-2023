package aoc

case class Point3(x: Int, y: Int, z: Int) extends PointLike[Point3]:
  def *(n: Int): Point3 =
    Point3(x * n, y * n, z * n)
  def +(that: Point3): Point3 =
    Point3(x + that.x, y + that.y, z + that.z)
  def -(that: Point3): Point3 =
    Point3(x - that.x, y - that.y, z - that.z)
  def manhattanDistance(that: Point3): Int =
    (x - that.x).abs + (y - that.y).abs + (z - that.z).abs
  def offset(that: Point3, count: Int): Point3 =
    this + (that * count)
  def min(that: Point3): Point3 =
    Point3(x min that.x, y min that.y, z min that.z)
  def max(that: Point3): Point3 =
    Point3(x max that.x, y max that.y, z max that.z)
  def <=(that: Point3): Boolean =
    x <= that.x && y <= that.y && z <= that.z
end Point3
object Point3:
  given ordering: Ordering[Point3] = Ordering.by(p => (p.x, p.y, p.z))

case class Area3(min: Point3, max: Point3):
  def iterator: Iterator[Point3] =
    for
      x <- (min.x to max.x).iterator
      y <- (min.y to max.y).iterator
      z <- (min.z to max.z).iterator
    yield Point3(x, y, z)

  def intersect(that: Area3): Option[Area3] =
    val intersectMin = min max that.min
    val intersectMax = max min that.max
    Option.when(intersectMin <= intersectMax)(Area3(intersectMin, intersectMax))

  def size[A](using cNumeric: Numeric[A]): A =
    import scala.math.Numeric.Implicits.infixNumericOps
    val d = max - min + Point3(1, 1, 1)
    cNumeric.fromInt(d.x) * cNumeric.fromInt(d.y) * cNumeric.fromInt(d.z)
end Area3
