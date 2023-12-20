package aoc.solutions
import aoc.*
import scala.annotation.tailrec

class Day13 extends AocTest:

  val rockMaps =
    input
      .mkString("\n")
      .split("\n\n")
      .map { s =>
        val grid   = s.split("\n").toVector.map(_.toVector)
        val area   = Area(grid)
        val points = area.points.filter(grid(_) == '#').toSet
        (points -> area)
      }
      .toSeq
  end rockMaps

  def findMirror(points: Set[Point], area: Area, smudge: Int): Option[Int] =
    val flipped = points.map(p => Point(area.right - p.x, p.y))
    val shifts  = 1 - area.right until area.right by 2
    val mirrorShift = shifts.find: shift =>
      val shifted       = flipped.map(p => p.copy(x = p.x + shift))
      val intersectArea = area.intersect(Area.bounding(shifted)).get
      val intersect     = shifted.intersect(points)
      val leftDiff      = points.filter(intersectArea.contains).diff(intersect).size == smudge
      val rightDiff     = shifted.filter(intersectArea.contains).diff(intersect).size == smudge
      leftDiff && rightDiff

    mirrorShift.map(shift => (area.xRange.end + shift) / 2)
  end findMirror

  def mirrors(points: Set[Point], area: Area, smudge: Int) =
    val vertical        = findMirror(points, area, smudge).getOrElse(0)
    val transposePoints = points.map(p => p.copy(x = p.y, y = p.x))
    val transposeArea   = Area(area.yRange, area.xRange)
    val horizontal      = findMirror(transposePoints, transposeArea, smudge).getOrElse(0)
    (horizontal, vertical)

  def sum(smudge: Int) =
    val (horizontal, vertical) = rockMaps.map(mirrors(_, _, smudge)).unzip
    horizontal.sum * 100 + vertical.sum

  test("part1"):
    assertEquals(sum(0), 33735)
  test("part2"):
    assertEquals(sum(1), 38063)

end Day13
