package aoc.`2023`

import scala.annotation.tailrec
import aoc.*

class Day11 extends AocTest:
  val starMap: Area             = Area(input)
  val galaxies: Vector[Point]   = starMap.pointsIterator.filter(input(_) == '#').toVector
  val occupiedRows: Set[Int]    = galaxies.map(_.y).toSet
  val occupiedColumns: Set[Int] = galaxies.map(_.x).toSet
  def measure(a: Point, b: Point, growth: Long): Long =
    val bounds: Area      = Area.bounding(a, b)
    val emptyColumns: Int = bounds.xRange.count(!occupiedColumns(_))
    val emptyRows: Int    = bounds.yRange.count(!occupiedRows(_))
    a.manhattanDistance(b) + (emptyColumns + emptyRows) * (growth - 1)
  def trips(growth: Long) = galaxies.combinations(2).collect { case Vector(a, b) => measure(a, b, growth) }
  test("part1"):
    assertEquals(trips(2).sum, 9214785L)
  test("part2"):
    assertEquals(trips(1000000).sum, 613686987427L)
end Day11
