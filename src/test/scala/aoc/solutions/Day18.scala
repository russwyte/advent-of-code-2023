package aoc.solutions
import aoc.*
import aoc.Direction.*
import collection.mutable
class Day18 extends AocTest:

  case class Step(direction: Direction, length: Int, rgb: String)

  type Extractor = String => Step

  def size(extractor: Extractor): Long =
    val steps = input.map(extractor)
    val points = steps.scanLeft(Point(0, 0))({ case (p, step) =>
      p.move(step.direction, step.length)
    })
    val area     = Geometry.polygonArea[Long](points) // shoelace
    val boundary = steps.map(_.length.toLong).sum
    val inside =
      area - (boundary / 2) + 1 // Using Pick's theorem (area = interior + boundary / 2 - 1) solve for interior (interior = area - boundary / 2 + 1)
    boundary + inside
  end size

  def part1(s: String): Step = s match
    case s"$direction $length (#$rgb)" => Step(Direction.fromChar(direction.head), length.toInt, rgb)

  def part2(s: String): Step =
    val d = s.init.last.asDigit match
      case 0 => R
      case 1 => D
      case 2 => L
      case 3 => U
      case _ => ???
    val l = Integer.parseInt(s.init.init.takeRight(5), 16)
    Step(d, l, "")
  end part2

  test("part1"):
    assertEquals(size(part1), 70253L)
  test("part2"):
    assertEquals(size(part2), 131265059885080L)
end Day18
