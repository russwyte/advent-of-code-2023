package aoc.`2023`
import aoc.*
import scala.annotation.tailrec
import aoc.Direction.*
class Day14 extends AocTest:
  case class Table(grid: IndexedSeq[IndexedSeq[Char]]):
    val area          = Area(grid)
    val immobileRocks = area.pointsIterator().filter(grid(_) == '#').toSet
    def slideAllMobileRocks(direction: Direction): Table =
      val toMove = area.pointsIterator(direction).filter(grid(_) == 'O')
      @tailrec
      def roll(rock: Point, stopped: Set[Point]): Set[Point] =
        val next = rock.move(direction)
        if !area.contains(next) || immobileRocks.contains(next) || stopped.contains(next) then stopped + rock
        else roll(next, stopped)
      val slidRocks = toMove.foldLeft(Set.empty[Point])((stopped, rock) => stopped union roll(rock, stopped))
      val clearGrid = grid.map { row =>
        row.map { c =>
          if c == 'O' then '.'
          else c
        }
      }
      Table(area.draw { p =>
        if slidRocks.contains(p) then 'O'
        else clearGrid(p)
      })
    end slideAllMobileRocks
    def sumRocks(direction: Direction) = area
      .pointsIterator(direction)
      .filter(grid(_) == 'O')
      .map { p =>
        direction match
          case Up | North   => area.height - p.y
          case Down | South => p.y
          case Left | West  => p.x
          case Right | East => area.width - p.x
          case Stop         => 0
      }
      .sum
    override def toString(): String = area.draw(p => grid(p))
  end Table

  object Table:
    def apply(s: String): Table          = Table(s.split('\n').map(_.toVector).toVector)
    def apply(sv: Vector[String]): Table = Table(sv.map(_.toVector))

  val example1 = Table("""O....#....
    |O.OO#....#
    |.....##...
    |OO.#O....O
    |.O.....O#.
    |O.#..O.#.#
    |..O..#O..O
    |.......O..
    |#....###..
    |#OO..#....""".stripMargin)

  test("Part 1"):
    assertEquals(example1.slideAllMobileRocks(N).sumRocks(N), 136)
    assertEquals(Table(input).slideAllMobileRocks(N).sumRocks(N), 106648)
  test("Part 2"):
    def spin(table: Table) =
      table.slideAllMobileRocks(N).slideAllMobileRocks(W).slideAllMobileRocks(S).slideAllMobileRocks(E)

    // we need to find a cycle to find the stable state
    // we cannot simply loop because n might be huge
    def findTableAt(table: Table, n: Long): Table =
      def findTableAtRec(currentTable: Table, currentStep: Long, visited: Map[Table, Long], order: List[Table]): Table =
        if visited.contains(currentTable) then
          val cycleStart     = visited(currentTable)
          val cycleLength    = currentStep - cycleStart
          val remainingSteps = (n - cycleStart) % cycleLength
          if remainingSteps == 0 then currentTable
          else order(cycleStart.toInt + remainingSteps.toInt)
        else
          findTableAtRec(
            spin(currentTable),
            currentStep + 1,
            visited + (currentTable -> currentStep),
            order :+ currentTable,
          )

      findTableAtRec(table, 0, Map(), List())
    end findTableAt
    assertEquals(findTableAt(Table(input), 1000000000L).sumRocks(N), 87700)
end Day14
