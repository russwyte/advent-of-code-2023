package aoc.geo

import scala.math.*
import Integral.Implicits.infixIntegralOps
import Ordering.Implicits.infixOrderingOps

case class Point2[A](x: A, y: A)(using i: Integral[A]):
  import Point2.Offsets.*
  def +(that: Point2[A]): Point2[A] = Point2(x + that.x, y + that.y)
  def -(that: Point2[A]): Point2[A] = Point2(x - that.x, y - that.y)
  def *(that: Point2[A]): Point2[A] = Point2(x * that.x, y * that.y)
  def /(that: Point2[A]): Point2[A] = Point2(x / that.x, y / that.y)

  def +(that: A): Point2[A] = Point2(x + that, y + that)
  def -(that: A): Point2[A] = Point2(x - that, y - that)
  def *(that: A): Point2[A] = Point2(x * that, y * that)
  def /(that: A): Point2[A] = Point2(x / that, y / that)

  def offset(that: Point2[A], count: A): Point2[A] = this + (that * count)

  def dot(that: Point2[A]): A   = x * that.x + y * that.y
  def cross(that: Point2[A]): A = x * that.y - y * that.x

  def <(that: Point2[A]): Boolean  = x < that.x && y < that.y
  def <=(that: Point2[A]): Boolean = x <= that.x && y <= that.y
  def >(that: Point2[A]): Boolean  = x > that.x && y > that.y
  def >=(that: Point2[A]): Boolean = x >= that.x && y >= that.y

  def manhattanDistance(that: Point2[A]): A =
    (x - that.x).abs + (y - that.y).abs

  def rotateRight: Point2[A] = Point2(y, -x)
  def rotateLeft: Point2[A]  = Point2(-y, x)

  def adjacent: Set[Point2[A]] =
    Set(copy(y = y - i.one), copy(y = y + i.one), copy(x = x - i.one), copy(x = x + i.one))
end Point2
object Point2:
  object Offsets:
    def North[A](using i: Integral[A]) = Point2(i.zero, -i.one)
    def South[A](using i: Integral[A]) = Point2(i.zero, i.one)
    def East[A](using i: Integral[A])  = Point2(i.one, i.zero)
    def West[A](using i: Integral[A])  = Point2(-i.one, i.zero)
  given [A](using Integral[A]): Ordering[Point2[A]] =
    Ordering.by(p => (p.x, p.y))
  extension [A](a: A)(using Integral[A])
    def +(point: Point2[A]) = point + a
    def -(point: Point2[A]) = Point2(a, a) - point
    def *(point: Point2[A]) = point * a
    def /(point: Point2[A]) = Point2(a, a) / point
end Point2
