package aoc.solutions

import aoc.*

import scala.util.control.TailCalls.*
import scala.annotation.tailrec
import aoc.graph.*
import aoc.graph.Dijkstra.traverse
import scala.concurrent.duration.Duration

class Day23 extends AocTest:
  override val munitTimeout = Duration(120, "min")
  enum Tile:
    def char: Char =
      this match
        case Forest     => '#'
        case Path       => '.'
        case SlopeNorth => '^'
        case SlopeSouth => 'v'
        case SlopeEast  => '>'
        case SlopeWest  => '<'
        case Start      => 'S'
        case Goal       => 'G'
    case Forest, Path, SlopeNorth, SlopeSouth, SlopeEast, SlopeWest, Start, Goal
  end Tile
  object Tile:
    def apply(c: Char): Tile =
      c match
        case '#' => Forest     // can't move to this tile
        case '.' => Path
        case '^' => SlopeNorth // can only move north
        case 'v' => SlopeSouth // can only move south
        case '>' => SlopeEast  // can only move east
        case '<' => SlopeWest  // can only move west
        case 'S' => Start
        case 'G' => Goal
  end Tile

  case class Maze(grid: Grid[Tile], isSteep: Boolean = true):
    import Direction.*

    val area = Area(grid)
    def tileAt(p: Point): Tile =
      import Tile.*
      val tile = grid(p)
      tile match
        case SlopeNorth | SlopeSouth | SlopeEast | SlopeWest if isSteep  => tile
        case SlopeNorth | SlopeSouth | SlopeEast | SlopeWest if !isSteep => Path
        case Path if area.yRange.start == p.y                            => Start
        case Path if area.yRange.end - 1 == p.y                          => Goal
        case _                                                           => tile
    end tileAt
    override def toString(): String = area.draw: p =>
      tileAt(p).char
    case class Step(direction: Direction, start: Point, end: Point)
    def possibleSteps(p: Point): Set[Step] =
      val tile = tileAt(p)
      val res =
        tile match
          case Tile.SlopeNorth => Set(Step(North, p, p.move(North)))
          case Tile.SlopeSouth => Set(Step(South, p, p.move(South)))
          case Tile.SlopeEast  => Set(Step(East, p, p.move(East)))
          case Tile.SlopeWest  => Set(Step(West, p, p.move(West)))
          case Tile.Start      => Set(Step(South, p, p.move(South)))
          case Tile.Path =>
            p.adjacentMap.collect {
              case (d, pp) if area.contains(pp) && tileAt(pp) != Tile.Forest => Step(d, p, pp)
            }.toSet
          case _ =>
            Set.empty // notice that we don't return a step for the goal tile because we don't want to move past it
      res
    end possibleSteps
    val start = area.pointsIterator(North).find(tileAt(_) == Tile.Start).get
    val goal  = area.pointsIterator(South).find(tileAt(_) == Tile.Goal).get
    val branching = area
      .pointsIterator(North)
      .filter { p =>
        val tile = tileAt(p)
        tile != Tile.Forest && possibleSteps(p).size > 2
      }
      .toSet
    val keyPoints = branching + start + goal
    def findLongest: Int =
      val keyAdjacentPoints: Map[Point, Map[Point, Int]] =
        (branching + start).view
          .map({ from =>
            val graphTraversal = new GraphTraversal[Point] with UnitNeighbors[Point]:
              override val startNode: Point = from

              override def unitNeighbors(p: Point): IterableOnce[Point] =
                if p != from && keyPoints(p) then Iterator.empty
                else possibleSteps(p).map(_.end)
            val distances = BFS.traverse(graphTraversal).distances
            val keyDistances = distances
              .filter({ (to, _) => to != from && keyPoints(to) })
              .toMap

            from -> keyDistances
          })
          .toMap

      // not stack safe but it works - slowly
      def solver(p: Point, keyAdjacentsPoints: Map[Point, Map[Point, Int]], distance: Int): Int =
        if p == goal then distance
        else
          keyAdjacentsPoints.get(p) match
            case Some(adjacents) =>
              val updated = keyAdjacentsPoints - p
              (for (toPos, cost) <- adjacents
              yield solver(toPos, updated, distance + cost)).max
            case None => 0

      solver(start, keyAdjacentPoints, 0)
    end findLongest

  end Maze

  object Maze:
    def apply(str: String): Maze =
      Maze(Grid(Tile.apply)(str))

  val example = Maze("""#.#####################
      |#.......#########...###
      |#######.#########.#.###
      |###.....#.>.>.###.#.###
      |###v#####.#v#.###.#.###
      |###.>...#.#.#.....#...#
      |###v###.#.#.#########.#
      |###...#.#.#.......#...#
      |#####.#.#.#######.#.###
      |#.....#.#.#.......#...#
      |#.#####.#.#.#########v#
      |#.#...#...#...###...>.#
      |#.#.#v#######v###.###v#
      |#...#.>.#...>.>.#.###.#
      |#####v#.#.###v#.#.###.#
      |#.....#...#...#.#.#...#
      |#.#########.###.#.#.###
      |#...###...#...#...#.###
      |###.###.#.###v#####v###
      |#...#...#.#.>.>.#.>.###
      |#.###.###.#.###.#.#v###
      |#.....###...###...#...#
      |#####################.#""".stripMargin)

  val maze = Maze(inputString)

  def part1(maze: Maze): Int = maze.findLongest

  def part2(maze: Maze): Int = maze.copy(isSteep = false).findLongest

  test("part1"):
    assertEquals(part1(example), 94)
    assertEquals(part1(maze), 2326)

  test("part2"):
    assertEquals(part2(example), 154)
    assertEquals(part2(maze), 6574)
end Day23
