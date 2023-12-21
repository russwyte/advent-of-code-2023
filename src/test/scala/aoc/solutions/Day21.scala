package aoc.solutions
import aoc.*
import scala.annotation.targetName
import scala.annotation.tailrec
import scala.collection.immutable.Queue

class Day21 extends AocTest:
  enum Tile:
    def char = this match
      case Open  => '.'
      case Rock  => '#'
      case Start => 'S'
    case Open, Rock, Start
  object Tile:
    def apply(c: Char): Tile = c match
      case '.' => Open
      case '#' => Rock
      case 'S' => Start
  case class Garden(grid: Grid[Tile]):
    import Tile.*
    val area  = Area(grid)
    val start = area.pointsIterator(Direction.North).find(p => grid(p) == Start).get

    def wrap(p: Point): Point =
      val newX = (p.x % area.xRange.end + area.xRange.end) % area.xRange.end
      val newY = (p.y % area.yRange.end + area.yRange.end) % area.yRange.end
      Point(newX, newY)

    def notRock(p: Point) = grid(wrap(p)) != Rock

    def wrappedAdjacent(p: Point): Set[Point] =
      Set(p.move(Direction.Up), p.move(Direction.Down), p.move(Direction.Left), p.move(Direction.Right))
        .filter(notRock)

    def walk(p: Point) = wrappedAdjacent(p)

    def extentFromStartInSteps(steps: Int): Set[Point] =
      def loop(steps: Int, points: Set[Point]): Set[Point] =
        if steps == 0 then points
        else loop(steps - 1, points.flatMap(walk))
      loop(steps, Set(start))

    override def toString(): String = area.draw { p => grid(p).char }

  end Garden

  object Garden:
    @targetName("applyWithVector")
    def apply(input: IndexedSeq[String]): Garden =
      Garden(Grid(Tile.apply)(input))
    def apply(input: String): Garden = apply(input.split("\n").toIndexedSeq)

  val garden = Garden(input)
  val example = Garden("""...........
      |.....###.#.
      |.###.##..#.
      |..#.#...#..
      |....#.#....
      |.##..S####.
      |.##..#...#.
      |.......##..
      |.##.#.####.
      |.##..##.##.
      |...........""".stripMargin)

  test("part1"):
    assertEquals(example.extentFromStartInSteps(6).size, 16)
    assertEquals(garden.extentFromStartInSteps(64).size, 3809)
  test("part2"):
    // this is pretty inefficent - but it works in a couple seconds - will optimize later
    // our input happens to be 131 * 131 so we can use that to our advantage here with a quadratic equation
    val x: Long = 26501365 / 131
    val r       = 26501365 % 131
    val y1      = garden.extentFromStartInSteps(r).size
    val y2      = garden.extentFromStartInSteps(r + 131).size
    val y3      = garden.extentFromStartInSteps(r + 131 * 2).size
    val res =
      y1 * (x - 1) * (x - 2) / -1 / -2 +
        y2 * x * (x - 2) / 1 / -1 +
        y3 * x * (x - 1) / 2 / 1
    assertEquals(res, 629720570456311L)

end Day21
