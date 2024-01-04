package aoc.geo

import scala.math.*
import Integral.Implicits.infixIntegralOps
import Ordering.Implicits.infixOrderingOps

case class Point3[A](x: A, y: A, z: A)(using i: Integral[A]):
  def xy = Point2(x, y)
  def xz = Point2(x, z)

  def +(that: Point3[A]): Point3[A] = Point3(x + that.x, y + that.y, z + that.z)
  def -(that: Point3[A]): Point3[A] = Point3(x - that.x, y - that.y, z - that.z)
  def *(that: Point3[A]): Point3[A] = Point3(x * that.x, y * that.y, z * that.z)
  def /(that: Point3[A]): Point3[A] = Point3(x / that.x, y / that.y, z / that.z)

  def +(that: A): Point3[A] = Point3(x + that, y + that, z + that)
  def -(that: A): Point3[A] = Point3(x - that, y - that, z - that)
  def *(that: A): Point3[A] = Point3(x * that, y * that, z * that)
  def /(that: A): Point3[A] = Point3(x / that, y / that, z / that)

  def offset(that: Point3[A], count: A): Point3[A] = this + (that * count)

  def dot(that: Point3[A]): A   = x * that.x + y * that.y + z * that.z
  def cross(that: Point3[A]): A = x * that.y - y * that.x

  def <(that: Point3[A]): Boolean  = x < that.x && y < that.y && z < that.z
  def <=(that: Point3[A]): Boolean = x <= that.x && y <= that.y && z <= that.z
  def >(that: Point3[A]): Boolean  = x > that.x && y > that.y && z > that.z
  def >=(that: Point3[A]): Boolean = x >= that.x && y >= that.y && z >= that.z

  def manhattanDistance(that: Point3[A]): A =
    (x - that.x).abs + (y - that.y).abs + (z - that.z).abs
end Point3

object Point3:
  given [A](using Integral[A]): Ordering[Point3[A]] =
    Ordering.by(p => (p.x, p.y, p.z))
  extension [A](a: A)(using Integral[A])
    def +(point: Point3[A]) = point + a
    def -(point: Point3[A]) = Point3(a, a, a) - point
    def *(point: Point3[A]) = point * a
    def /(point: Point3[A]) = Point3(a, a, a) / point
end Point3
