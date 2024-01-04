package aoc.geo
import Point2.*

class Point2Specs extends munit.FunSuite:
  test("Point2[Int] + Point2[Int]"):
    val p1 = Point2(1, 2)
    val p2 = Point2(3, 4)
    val p3 = Point2(-5, -10)
    assertEquals(p1 + p2, Point2(4, 6))
    assertEquals(p2 + p1, Point2(4, 6))
    assertEquals(p1 + p3, Point2(-4, -8))
  test("Point2[Int] - Point2[Int]"):
    val p1 = Point2(1, 2)
    val p2 = Point2(3, 4)
    assertEquals(p1 - p2, Point2(-2, -2))
    assertEquals(p2 - p1, Point2(2, 2))
  test("Point2[Int] * Point2[Int]"):
    val p1 = Point2(1, 2)
    val p2 = Point2(3, 4)
    assertEquals(p1 * p2, Point2(3, 8))
    assertEquals(p2 * p1, Point2(3, 8))
  test("Point2[Int] / Point2[Int]"):
    val p1 = Point2(1, 2)
    val p2 = Point2(3, 4)
    assertEquals(p1 / p2, Point2(0, 0))
    assertEquals(p2 / p1, Point2(3, 2))
  test("Point2[Int] + Int"):
    val p1 = Point2(1, 2)
    assertEquals(p1 + 3, Point2(4, 5))
    assertEquals(3 + p1, Point2(4, 5))
  test("Point2[Int] - Int"):
    val p1 = Point2(1, 2)
    assertEquals(p1 - 3, Point2(-2, -1))
    assertEquals(3 - p1, Point2(2, 1))
  test("Point2[Int] * Int"):
    val p1 = Point2(1, 2)
    assertEquals(p1 * 3, Point2(3, 6))
    assertEquals(3 * p1, Point2(3, 6))
  test("Point2[Int] / Int"):
    val p1 = Point2(1, 2)
    assertEquals(p1 / 3, Point2(0, 0))
    assertEquals(3 / p1, Point2(3, 1))
  test("Point2[Int] offset"):
    val p1 = Point2(1, 2)
    val p2 = Point2(3, 4)
    val p3 = Point2(-5, -10)
    assertEquals(p1 offset (p2, 3), Point2(10, 14))
    assertEquals(p1 offset (p3, 3), Point2(-14, -28))
  test("Point2[Int] dot"):
    val p1 = Point2(1, 2)
    val p2 = Point2(3, 4)
    assertEquals(p1 dot p2, 11)
  test("Point2[Int] cross"):
    val p1 = Point2(1, 2)
    val p2 = Point2(3, 4)
    assertEquals(p1 cross p2, -2)
  test("Point2[Int] <"):
    val p1 = Point2(1, 2)
    val p2 = Point2(3, 4)
    assertEquals(p1 < p2, true)
    assertEquals(p2 < p1, false)
  test("Point2[Int] <="):
    val p1 = Point2(1, 2)
    val p2 = Point2(3, 4)
    val p3 = Point2(1, 1)
    assertEquals(p1 <= p2, true)
    assertEquals(p2 <= p1, false)
    assertEquals(p1 <= p3, false)
    assertEquals(p3 <= p1, true)
  test("Point2[Int] >"):
    val p1 = Point2(1, 2)
    val p2 = Point2(3, 4)
    assertEquals(p1 > p2, false)
    assertEquals(p2 > p1, true)
  test("Point2[Int] >="):
    val p1 = Point2(1, 2)
    val p2 = Point2(3, 4)
    val p3 = Point2(1, 1)
    assertEquals(p1 >= p2, false)
    assertEquals(p2 >= p1, true)
    assertEquals(p1 >= p3, true)
    assertEquals(p3 >= p1, false)
  test("Point2[Int] manhattanDistance"):
    assertEquals(Point2(1, 2) manhattanDistance Point2(3, 4), 4)
    assertEquals(Point2(2, 2) manhattanDistance Point2(1, 2), 1)
    assertEquals(Point2(2, 2) manhattanDistance Point2(2, 2), 0)
  test("Point2[Int] rotateRight"):
    assertEquals(Point2(1, 2).rotateRight, Point2(2, -1))
  test("Point2[Int] rotateLeft"):
    assertEquals(Point2(1, 2).rotateLeft, Point2(-2, 1))
  test("Point2[Int] adjacent"):
    assertEquals(Point2(1, 2).adjacent, Set(Point2(1, 1), Point2(1, 3), Point2(0, 2), Point2(2, 2)))
  test("sorting Point2s"):
    val points = List(Point2(1, 2), Point2(3, 4), Point2(1, 1), Point2(2, 2))
    assertEquals(points.sorted, List(Point2(1, 1), Point2(1, 2), Point2(2, 2), Point2(3, 4)))
end Point2Specs
