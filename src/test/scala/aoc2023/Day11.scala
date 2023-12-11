package aoc2023

import aoc.*

class Day11 extends AocTest:
  val in              = input.toVector
  val starMap         = Area(in)
  val galaxies        = starMap.pointsIterator.filter(in(_) == '#').toVector
  val occupiedRows    = galaxies.map(_.y).toSet
  val occupiedColumns = galaxies.map(_.x).toSet
  def measure(a: Point, b: Point, growth: Long): Long =
    val bounds       = Area.bounding(a, b)
    val emptyColumns = bounds.xRange.count(!occupiedColumns(_))
    val emptyRows    = bounds.yRange.count(!occupiedRows(_))
    a.manhattanDistance(b) + (emptyColumns + emptyRows) * (growth - 1)
  def trips(growth: Long) = galaxies.combinations(2).collect { case Vector(a, b) => measure(a, b, growth) }
  test("part1"):
    assertEquals(trips(2).sum, 9214785L)
  test("part2"):
    assertEquals(trips(1000000).sum, 613686987427L)
end Day11
