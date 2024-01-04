package aoc.geo

import Integral.Implicits.infixIntegralOps
import Ordering.Implicits.infixOrderingOps
import scala.collection.immutable.NumericRange
import scala.collection.immutable.NumericRange.{Inclusive, Exclusive}
import Line2.*

case class Line2[A](start: Point2[A], end: Point2[A])(using intergral: Integral[A], doubleToA: DoubleRounder[A]):
  def manhattanDistance: A = start.manhattanDistance(end)

  def dx: A = end.x - start.x
  def dy: A = end.y - start.y

  private def emptyRange = Exclusive(intergral.zero, intergral.zero, intergral.one)

  def slope: Double = if isVertical then Double.PositiveInfinity else dy.toDouble / dx.toDouble

  def is45Degree: Boolean = slope.abs == 1.0

  def xRangeAt(y: A): NumericRange[A] =
    if (isHorizontal || isVertical) && yRange.contains(y) then xRange
    else if yRange.contains(y) then
      val x1: A = doubleToA(((y.toDouble - 0.5) - yIntercept) / slope)
      val x2: A = if is45Degree then doubleToA(((y.toDouble + 0.5) - yIntercept) / slope) else x1
      val calcRange: Inclusive[A] =
        Line2(Point2(x1, y), Point2(x2, y)).xRange
      val as = calcRange.intersect(xRange)
      Inclusive(as.head, as.last, calcRange.step)
    else emptyRange

  def yRangeAt(x: A): NumericRange[A] =
    if (isHorizontal || isVertical) && xRange.contains(x) then yRange
    else if xRange.contains(x) then
      val y1: A = doubleToA(((x.toDouble - 0.5) * slope) + yIntercept)
      val y2: A = if is45Degree then doubleToA(((x.toDouble + 0.5) * slope) + yIntercept) else y1
      val calcRange: Inclusive[A] =
        Line2(Point2(x, y1), Point2(x, y2)).yRange
      val as = calcRange.intersect(yRange)
      Inclusive(as.head, as.last, calcRange.step)
    else emptyRange

  def yIntercept: Double = start.y.toDouble - slope * start.x.toDouble

  def isVertical: Boolean   = dx == intergral.zero
  def isHorizontal: Boolean = dy == intergral.zero

  def xRange: Inclusive[A] = Inclusive(
    start.x,
    end.x,
    if dx.sign == intergral.zero then intergral.one else dx.sign,
  )
  def yRange: Inclusive[A] = Inclusive(
    start.y,
    end.y,
    if dy.sign == intergral.zero then intergral.one else dy.sign,
  )

  def pointsIntersecting = xRange.iterator.flatMap(x => yRangeAt(x).map(y => Point2(x, y)))

  def intersect(that: Line2[A]): Option[Point2[A]] =
    if isVertical && that.isVertical then None
    else if isHorizontal && that.isHorizontal then None
    else if isVertical then
      val x = start.x.toDouble
      val y = that.slope * x + that.yIntercept
      Some(Point2(doubleToA(x), doubleToA(y)))
    else if that.isVertical then
      val x = that.start.x.toDouble
      val y = slope * x + yIntercept
      Some(Point2(doubleToA(x), doubleToA(y)))
    else if slope == that.slope then None
    else
      val x = (that.yIntercept - yIntercept) / (slope - that.slope)
      val y = slope * x + yIntercept
      Some(Point2(doubleToA(x), doubleToA(y)))
end Line2

case class Line3[A](start: Point3[A], end: Point3[A])(using intergral: Integral[A], doubleToA: DoubleRounder[A]):
  def manhattanDistance: A = start.manhattanDistance(end)

  def dx: A = end.x - start.x
  def dy: A = end.y - start.y
  def dz: A = end.z - start.z

  private def emptyRange = Exclusive(intergral.zero, intergral.zero, intergral.one)
