package aoc.geo

import scala.collection.immutable.NumericRange.Inclusive

class Line2Specs extends munit.FunSuite:
  test("Line2[Int] manhattanDistance"):
    val l1 = Line2(Point2(1, 2), Point2(3, 4))
    val l2 = Line2(Point2(3, 4), Point2(1, 2))
    assertEquals(l1.manhattanDistance, 4)
    assertEquals(l2.manhattanDistance, 4)
  test("Line2[Int] dx"):
    val l1 = Line2(Point2(1, 2), Point2(3, 4))
    val l2 = Line2(Point2(3, 4), Point2(1, 2))
    assertEquals(l1.dx, 2)
    assertEquals(l2.dx, -2)
  test("Line2[Int] dy"):
    val l1 = Line2(Point2(1, 2), Point2(3, 4))
    val l2 = Line2(Point2(3, 4), Point2(1, 2))
    assertEquals(l1.dy, 2)
    assertEquals(l2.dy, -2)
  test("Line2[Int] slope"):
    val l1         = Line2(Point2(1, 2), Point2(3, 4))
    val l2         = Line2(Point2(1, 4), Point2(3, 2))
    val vertical   = Line2(Point2(1, 2), Point2(1, 4))
    val horizontal = Line2(Point2(1, 2), Point2(3, 2))
    assertEquals(l1.slope, 1.0)
    assertEquals(l2.slope, -1.0)
    assertEquals(vertical.slope, Double.PositiveInfinity)
    assertEquals(horizontal.slope, 0.0)
  test("Line2[Int] yIntercept"):
    val l1 = Line2(Point2(1, 2), Point2(3, 4))
    val l2 = Line2(Point2(3, 4), Point2(1, 2))
    val l3 = Line2(Point2(1, 4), Point2(3, 2))
    val l4 = Line2(Point2(1, 2), Point2(3, 4))
    val l5 = Line2(Point2(-8, 3), Point2(3, -8))
    assertEquals(l1.yIntercept, 1.0)
    assertEquals(l2.yIntercept, 1.0)
    assertEquals(l3.yIntercept, 5.0)
    assertEquals(l4.yIntercept, 1.0)
    assertEquals(l5.yIntercept, -5.0)
  test("Line2[Int] isVertical"):
    val l1 = Line2(Point2(1, 2), Point2(3, 4))
    val l2 = Line2(Point2(3, 4), Point2(1, 2))
    val l3 = Line2(Point2(1, 2), Point2(1, 4))
    assertEquals(l1.isVertical, false)
    assertEquals(l2.isVertical, false)
    assertEquals(l3.isVertical, true)
  test("Line2[Int] isHorizontal"):
    val l1 = Line2(Point2(1, 2), Point2(3, 4))
    val l2 = Line2(Point2(3, 4), Point2(1, 2))
    val l3 = Line2(Point2(1, 2), Point2(3, 2))
    assertEquals(l1.isHorizontal, false)
    assertEquals(l2.isHorizontal, false)
    assertEquals(l3.isHorizontal, true)
  test("Line2[Int] xRange"):
    val l1 = Line2(Point2(1, 2), Point2(3, 4))
    val l2 = Line2(Point2(3, 4), Point2(1, 2))
    val l3 = Line2(Point2(1, 2), Point2(1, 4))
    assertEquals(l1.xRange, Inclusive(1, 3, 1))
    assertEquals(l2.xRange, Inclusive(3, 1, -1))
    assertEquals(l3.xRange, Inclusive(1, 1, 1))
  test("Line2[Int] yRange"):
    val l1 = Line2(Point2(1, 2), Point2(3, 4))
    val l2 = Line2(Point2(3, 4), Point2(1, 2))
    val l3 = Line2(Point2(1, 2), Point2(3, 2))
    assertEquals(l1.yRange, Inclusive(2, 4, 1))
    assertEquals(l2.yRange, Inclusive(4, 2, -1))
    assertEquals(l3.yRange, Inclusive(2, 2, 1))
  test("Line2[Int] intersect"):
    val l1 = Line2(Point2(1, 2), Point2(3, 4))
    val l2 = Line2(Point2(3, 4), Point2(1, 2))
    val l3 = Line2(Point2(1, 4), Point2(3, 2))
    val l4 = Line2(Point2(1, 2), Point2(1, 4))
    val l5 = Line2(Point2(1, 2), Point2(3, 2))
    val l6 = Line2(Point2(1, 2), Point2(3, 4))
    val l7 = Line2(Point2(-8, 3), Point2(3, -8))
    assertEquals(l1.intersect(l2), None)
    assertEquals(l1.intersect(l3), Some(Point2(2, 3)))
    assertEquals(l1.intersect(l4), Some(Point2(1, 2)))
    assertEquals(l1.intersect(l5), Some(Point2(1, 2)))
    assertEquals(l1.intersect(l6), None)
    assertEquals(l1.intersect(l7), Some(Point2(-3, -2)))
  test("Line2[Int] intersect with fractional result"):
    val l1 = Line2(Point2(1, 1), Point2(3, 2))
    val l2 = Line2(Point2(2, 2), Point2(5, 6))
    assertEquals(l1.intersect(l2), Some(Point2(1, 1)))
    assertEquals(l2.intersect(l1), Some(Point2(1, 1)))
  test("Line2[Int] intersect with negative fractional result"):
    val l1 = Line2(Point2(1, 1), Point2(-4, -4))
    val l2 = Line2(Point2(0, -1), Point2(-2, 0))
    assertEquals(l1.intersect(l2), Some(Point2(-1, -1)))
    assertEquals(l2.intersect(l1), Some(Point2(-1, -1)))
  test("Line2[Int] intersect with vertical line"):
    val l1 = Line2(Point2(1, 1), Point2(3, 2))
    val l2 = Line2(Point2(2, 2), Point2(2, 6))
    assertEquals(l1.intersect(l2), Some(Point2(2, 2)))
    assertEquals(l2.intersect(l1), Some(Point2(2, 2)))
  test("Line2[Int] intersect with horizontal line"):
    val l1 = Line2(Point2(1, 1), Point2(3, 2))
    val l2 = Line2(Point2(2, 2), Point2(5, 2))
    assertEquals(l1.intersect(l2), Some(Point2(3, 2)))
    assertEquals(l2.intersect(l1), Some(Point2(3, 2)))
  test("Line2[Int] xAt with vertical line"):
    val l1 = Line2(Point2(2, 2), Point2(2, 6))
    assertEquals(l1.xRangeAt(3), Inclusive(2, 2, 1))
    assertEquals(l1.xRangeAt(1).isEmpty, true)
  test("Line2[Int] xAt with horizontal line"):
    val l1 = Line2(Point2(2, 2), Point2(5, 2))
    assertEquals(l1.xRangeAt(2), Inclusive(2, 5, 1))
    assertEquals(l1.xRangeAt(1).isEmpty, true)
  test("Line2[Int] xAt with diagonal line"):
    val l1 = Line2(Point2(1, 1), Point2(3, 2))
    assertEquals(l1.xRangeAt(1), Inclusive(1, 2, 1))
    assertEquals(l1.xRangeAt(2), Inclusive(2, 3, 1))
    assertEquals(l1.xRangeAt(3).isEmpty, true)
  test("Line2[Int] yAt with vertical line"):
    val l1 = Line2(Point2(2, 2), Point2(2, 6))
    assertEquals(l1.yRangeAt(2), Inclusive(2, 6, 1))
    assertEquals(l1.yRangeAt(1).isEmpty, true)
  test("Line2[Int] yAt with horizontal line"):
    val l1 = Line2(Point2(2, 2), Point2(5, 2))
    assertEquals(l1.yRangeAt(2), Inclusive(2, 2, 1))
    assertEquals(l1.yRangeAt(1).isEmpty, true)
  test("Line2[Int] yAt with diagonal line"):
    val l1 = Line2(Point2(1, 1), Point2(3, 2))
    assertEquals(l1.yRangeAt(1), Inclusive(1, 1, 1))
    assertEquals(l1.yRangeAt(2), Inclusive(1, 2, 1))
    assertEquals(l1.yRangeAt(4).isEmpty, true)
  test("Line2[Int] pointsIntersecting with vertical line"):
    val l1 = Line2(Point2(2, 2), Point2(2, 6))
    assertEquals(l1.pointsIntersecting.toSet, Set(Point2(2, 2), Point2(2, 3), Point2(2, 4), Point2(2, 5), Point2(2, 6)))
  test("Line2[Int] pointsIntersecting with horizontal line"):
    val l1 = Line2(Point2(2, 2), Point2(5, 2))
    assertEquals(l1.pointsIntersecting.toSet, Set(Point2(2, 2), Point2(3, 2), Point2(4, 2), Point2(5, 2)))
  test("Line2[Int] pointsIntersecting with diagonal line"):
    val l1 = Line2(Point2(1, 1), Point2(3, 2))
    assertEquals(l1.pointsIntersecting.toSet, Set(Point2(1, 1), Point2(2, 1), Point2(2, 2), Point2(3, 2)))
  test("Line2[Int] pointsIntersecting with diagonal line with negative slope"):
    val l1 = Line2(Point2(1, 2), Point2(3, 1))
    assertEquals(l1.pointsIntersecting.toSet, Set(Point2(1, 2), Point2(2, 2), Point2(2, 1), Point2(3, 1)))
  test("Line2[Int] pointsIntersecting with diagonal line with negative slope and negative y"):
    val l1 = Line2(Point2(1, -2), Point2(3, -1))
    assertEquals(l1.pointsIntersecting.toSet, Set(Point2(1, -2), Point2(2, -2), Point2(2, -1), Point2(3, -1)))
  test("Line2[Int] pointsIntersecting with diagonal line with negative slope and negative x"):
    val l1 = Line2(Point2(-1, 2), Point2(-3, 1))
    assertEquals(l1.pointsIntersecting.toSet, Set(Point2(-1, 2), Point2(-2, 2), Point2(-2, 1), Point2(-3, 1)))
  test("Line2[Int] pointsIntersecting with diagonal line with negative slope and negative x and y"):
    val l1 = Line2(Point2(-1, -2), Point2(-3, -1))
    assertEquals(l1.pointsIntersecting.toSet, Set(Point2(-1, -2), Point2(-2, -2), Point2(-2, -1), Point2(-3, -1)))
  test("Line2[Int] pointsIntersecting with non 45 degree diagonal line"):
    val l1 = Line2(Point2(1, 1), Point2(3, 4))
    assertEquals(
      l1.pointsIntersecting.toSet,
      Set(Point2(1, 1), Point2(1, 2), Point2(2, 2), Point2(2, 3), Point2(3, 3), Point2(3, 4)),
    )
  test("Line2[Int] pointsIntersecting with non 45 degree diagonal line with negative slope"):
    val l1 = Line2(Point2(1, 4), Point2(3, 1))
    assertEquals(
      l1.pointsIntersecting.toSet,
      Set(Point2(1, 4), Point2(1, 3), Point2(2, 3), Point2(2, 2), Point2(3, 2), Point2(3, 1)),
    )
  test("Line2[Int] pointsIntersecting with non 45 degree diagonal line with negative slope and negative x"):
    val l1 = Line2(Point2(-1, 4), Point2(-3, 1))
    assertEquals(
      l1.pointsIntersecting.toSet,
      Set(Point2(-1, 4), Point2(-1, 3), Point2(-2, 3), Point2(-2, 2), Point2(-3, 2), Point2(-3, 1)),
    )
  test("Line2[Int] pointsIntersecting with non 45 degree diagonal line with negative slope and negative y"):
    val l1 = Line2(Point2(1, -4), Point2(3, -1))
    assertEquals(
      l1.pointsIntersecting.toSet,
      Set(Point2(1, -4), Point2(1, -3), Point2(2, -3), Point2(2, -2), Point2(3, -2), Point2(3, -1)),
    )
  test("Line2[Int] pointsIntersecting with non 45 degree diagonal line with negative slope and negative x and y"):
    val l1 = Line2(Point2(-1, -4), Point2(-3, -1))
    assertEquals(
      l1.pointsIntersecting.toSet,
      Set(Point2(-1, -4), Point2(-1, -3), Point2(-2, -3), Point2(-2, -2), Point2(-3, -2), Point2(-3, -1)),
    )
end Line2Specs
