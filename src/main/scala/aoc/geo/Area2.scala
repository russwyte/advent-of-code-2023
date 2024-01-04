package aoc.geo

import Integral.Implicits.infixIntegralOps
import Ordering.Implicits.infixOrderingOps
import scala.collection.immutable.NumericRange.Inclusive
import Grid.*

case class Area2[A](min: Point2[A], max: Point2[A])(using i: Integral[A], c: DoubleRounder[A]):
  def draw(f: Point2[A] => Char): String =
    val lines =
      for y <- min.y.toInt to max.y.toInt
      yield
        val line =
          for x <- min.x.toInt to max.x.toInt
          yield f(Point2(i.fromInt(x), i.fromInt(y)))
        line.mkString
    lines.mkString("\n")
  def width: A  = max.x - min.x + i.one
  def height: A = max.y - min.y + i.one
  def size: A   = width * height
  def topLine: Line2[A] =
    Line2(Point2(min.x, min.y), Point2(max.x, min.y))
  def bottomLine: Line2[A] =
    Line2(Point2(min.x, max.y), Point2(max.x, max.y))
  def leftLine: Line2[A] =
    Line2(Point2(min.x, min.y), Point2(min.x, max.y))
  def rightLine: Line2[A] =
    Line2(Point2(max.x, min.y), Point2(max.x, max.y))
  def contains(point: Point2[A]): Boolean =
    point >= min && point <= max

  def contains(line: Line2[A]): Boolean =
    contains(line.start) && contains(line.end)

  def contains(area: Area2[A]): Boolean =
    contains(area.min) && contains(area.max)

  def intersect(that: Area2[A]): Option[Area2[A]] =
    val min = Point2(
      i.max(this.min.x, that.min.x),
      i.max(this.min.y, that.min.y),
    )
    val max = Point2(
      i.min(this.max.x, that.max.x),
      i.min(this.max.y, that.max.y),
    )
    if min < max then Some(Area2(min, max)) else None
  end intersect

  def intersect(that: Line2[A]): Option[Line2[A]] =
    if contains(that) then Some(that)
    else
      val start = Point2(
        i.max(min.x, that.start.x),
        i.max(min.y, that.start.y),
      )
      val end = Point2(
        i.min(max.x, that.end.x),
        i.min(max.y, that.end.y),
      )
      if start < end then Some(Line2(start, end)) else None

  def intersect(that: Point2[A]): Option[Point2[A]] =
    if contains(that) then Some(that) else None
end Area2
object Area2:
  def apply[T](grid: Grid[T]): Area2[Int] =
    val min = Point2(0, 0)
    val max = Point2(grid.headOption.fold(0)(_.size - 1), grid.size - 1)
    Area2(min, max)
  def apply[A](left: A, right: A, top: A, bottom: A)(using i: Integral[A], c: DoubleRounder[A]): Area2[A] =
    Area2(Point2(left, top), Point2(right, bottom))
  extension [T](g: Grid[T]) def area: Area2[Int] = Area2(g)
  end extension

  @main def go() =
    import Grid.*
    val line = Line2(Point2(0, 0), Point2(10, 14))
    val grid = CharGrid(
      ".............",
      ".............",
      ".............",
      ".............",
      ".............",
      ".............",
      ".............",
      ".............",
      ".............",
      ".............",
      ".............",
      ".............",
      ".............",
    )
    // lets get points in the grid that represent the line

    val area   = grid.area
    val points = line.pointsIntersecting.toSet
    println(area.draw { x =>
      if points(x) then '*' else grid.get(x).getOrElse('.')
    })
  end go
end Area2
