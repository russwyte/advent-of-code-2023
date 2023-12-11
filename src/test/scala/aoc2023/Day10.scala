package aoc2023
import aoc.*
import Direction.*

class Day10 extends AocTest:
  import Day10.*
  val pipeField = PipeField(input.toVector)
  test("part1"):
    assertEquals(pipeField.loopSteps.length - 1, 6640)

  test("part2"):
    assertEquals(pipeField.inside.size, 411)
end Day10

object Day10:
  case class PipeField(in: Vector[String]):
    val area = Area(in)

    val start = area.pointsIterator.find(in(_) == 'S').get

    enum Tile:
      case TopBottom, TopLeft, TopRight, BottomLeft, BottomRight, LeftRight, Empty, Start

      def topOpen: Boolean = this match
        case TopBottom | TopLeft | TopRight | Start => true
        case _                                      => false

      def bottomOpen: Boolean = this match
        case TopBottom | BottomLeft | BottomRight => true
        case _                                    => false

      def leftOpen: Boolean = this match
        case TopLeft | BottomLeft | LeftRight | Start => true
        case _                                        => false

      def rightOpen: Boolean = this match
        case TopRight | BottomRight | LeftRight => true
        case _                                  => false

      def toChar: Char = this match
        case TopBottom   => '║'
        case TopLeft     => '╝'
        case TopRight    => '╚'
        case BottomLeft  => '╗'
        case BottomRight => '╔'
        case LeftRight   => '═'
        case Empty       => '.'
        case Start       => 'S'
    end Tile

    object Tile:
      def apply(c: Char) = c match
        case '7' => BottomLeft
        case 'J' => TopLeft
        case 'L' => TopRight
        case 'F' => BottomRight
        case '-' => LeftRight
        case '|' => TopBottom
        case 'S' => startTileType
        case _   => Empty
    end Tile

    import Tile.*

    val startTileType: Tile =
      val u = Tile(in(start.move(U)))
      val d = Tile(in(start.move(D)))
      val r = Tile(in(start.move(R)))
      val l = Tile(in(start.move(L)))
      if u.bottomOpen && d.topOpen then TopBottom
      else if u.bottomOpen && l.rightOpen then TopLeft
      else if u.bottomOpen && r.leftOpen then TopRight
      else if d.topOpen && l.rightOpen then BottomLeft
      else if d.topOpen && r.leftOpen then BottomRight
      else if l.rightOpen && r.leftOpen then LeftRight
      else Empty // should not happen
    end startTileType

    val grid: Map[Point, Tile] = Map.from[Point, Tile]:
      for
        (row, y)  <- in.zipWithIndex
        (char, x) <- row.zipWithIndex
      yield Point(x, y) -> Tile(char)

    def adjacent(p: Point): Set[Point] =
      val n =
        Option.when(area.contains(p.move(North)) && grid(p).topOpen && grid(p.move(North)).bottomOpen)(p.move(North))
      val s =
        Option.when(area.contains(p.move(South)) && grid(p).bottomOpen && grid(p.move(South)).topOpen)(p.move(South))
      val e =
        Option.when(area.contains(p.move(East)) && grid(p).rightOpen && grid(p.move(East)).leftOpen)(p.move(East))
      val w =
        Option.when(area.contains(p.move(West)) && grid(p).leftOpen && grid(p.move(West)).rightOpen)(p.move(West))
      List(n, s, e, w).flatten.toSet
    end adjacent

    val loopSteps: List[Set[Point]] =
      val steps = Iterator.unfold(Set.empty[Point] -> Set(start)):
        case (visited, current) =>
          Option.when(current.nonEmpty):
            val next = current.flatMap(adjacent).diff(visited)
            (current, current -> next)
      steps.toList

    val boundary = loopSteps.reduce(_ union _)

    def nextDir(pos: Point, dir: Direction) =
      (dir, grid(pos.move(dir))) match
        case (U, TopBottom)   => U
        case (U, BottomLeft)  => L
        case (U, BottomRight) => R
        case (D, TopBottom)   => D
        case (D, TopLeft)     => L
        case (D, TopRight)    => R
        case (R, LeftRight)   => R
        case (R, BottomLeft)  => D
        case (R, TopLeft)     => U
        case (L, LeftRight)   => L
        case (L, TopRight)    => U
        case (L, BottomRight) => D
        case (x, _)           => x
      end match
    end nextDir

    def walk =
      Iterator.unfold[(List[Point], List[Point]), (Point, Direction)]((start, U)):
        case (pos, dir) =>
          val next  = pos.move(dir)
          val left  = pos.move(dir.left)
          val right = pos.move(dir.right)
          Option.when(next != start):
            val leftSpace =
              List(left, left.move(dir))
                .filter(area.contains(_))
                .filterNot(boundary)
            val rightSpace =
              List(right, right.move(dir))
                .filter(area.contains(_))
                .filterNot(boundary)
            val newState = (next, nextDir(pos, dir))
            (leftSpace, rightSpace) -> newState

    val (left, right) = walk.toList.unzip

    def flood(seeds: Set[Point]): Set[Point] =
      val points = Iterator.unfold(Set.empty[Point] -> seeds):
        case (visited, current) =>
          Option.when(current.nonEmpty):
            val next = current.flatMap(p => area.adjacent(p)).diff(boundary).diff(visited)
            (current, current -> next)
      points.reduce(_ union _)

    val (inside, outside) =
      val leftSeeds = left.flatten.toSet

      val rightSeeds = right.flatten.toSet

      val floodLeft = flood(leftSeeds)

      val floodRight = flood(rightSeeds)

      if floodLeft.contains(Point(0, 0)) then (floodRight, floodLeft)
      else (floodLeft, floodRight)
    end val

    override def toString(): String =
      val dark  = '\u2593'
      val light = '\u2591'
      val block = '\u2588'

      area.draw: p =>
        if p == start then block
        else if inside(p) then dark
        else if outside(p) then light
        else if boundary.contains(p) then grid(p).toChar
        else 'X'
    end toString
  end PipeField
end Day10
