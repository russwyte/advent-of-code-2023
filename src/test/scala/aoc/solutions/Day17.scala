package aoc.solutions

import aoc.AocTest
import aoc.Direction.*
import aoc.*
import aoc.graph.*

class Day17 extends AocTest:
  val example =
    """2413432311323
      |3215453535623
      |3255245654254
      |3446585845452
      |4546657867536
      |1438598798454
      |4457876987766
      |3637877979653
      |4654967986887
      |4564679986453
      |1224686865563
      |2546548887735
      |4322674655533""".stripMargin
  case class Crucible(pos: Point, direction: Direction, directionCount: Int)

  case class Blocks(s: String):
    val grid      = s.split("\n").map(_.toVector.map(_.asDigit)).toVector
    val area      = Area(grid)
    val MaxLength = 3
    def validDirections(p: Point, current: Direction) =
      val res =
        if current == Stop then List(Up, Down, Left, Right)
        else current :: current.left :: current.right :: Nil
      res.filter(d => area.contains(p.move(d)))

    def leastHeatLoss(canMove: (Crucible, Direction) => Boolean, canStop: Crucible => Boolean): Int =

      val graphSearch = new GraphSearch[Crucible]:
        override val startNode: Crucible = Crucible(area.topLeft, Stop, 0)

        override def neighbors(crucible: Crucible): IterableOnce[(Crucible, Int)] =
          val Crucible(pos, direction, directionCount) = crucible
          for
            d <- validDirections(pos, direction)
            if canMove(crucible, d)
            newPos = pos.move(d)
            if area.contains(newPos)
            newDirectionCount = if d == direction then directionCount + 1 else 1
          yield Crucible(newPos, d, newDirectionCount) -> grid(newPos)
        end neighbors

        private val targetPos = area.bottomRight

        override def isTargetNode(crucible: Crucible, dist: Int): Boolean =
          crucible.pos == targetPos && canStop(crucible)

      Dijkstra.search(graphSearch).target.get._2
    end leastHeatLoss

  end Blocks

  val exampleBlocks = Blocks(example)
  val blocks        = Blocks(input.mkString("\n"))
  test("part 1"):
    def canMove(crucible: Crucible, direction: Direction): Boolean =
      crucible.directionCount < 3 || direction != crucible.direction
    assertEquals(exampleBlocks.leastHeatLoss(canMove, _ => true), 102)
    assertEquals(blocks.leastHeatLoss(canMove, _ => true), 959)

  test("part 2"):
    def canMove(crucible: Crucible, direction: Direction): Boolean =
      crucible.direction == Stop || (direction == crucible.direction && crucible.directionCount < 10) || (direction != crucible.direction && crucible.directionCount >= 4)
    def canStop(crucible: Crucible): Boolean = crucible.directionCount >= 4
    assertEquals(exampleBlocks.leastHeatLoss(canMove, canStop), 94)
    assertEquals(blocks.leastHeatLoss(canMove, canStop), 1135)

end Day17
