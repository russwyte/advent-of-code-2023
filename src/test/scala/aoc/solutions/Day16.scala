package aoc.solutions
import aoc.*
import aoc.Direction.*
import scala.annotation.tailrec
class Day16 extends AocTest:
  case class Grid(s: String):
    val reflectorGrid = s.split("\n").map(_.toVector).toVector
    val area          = Area(reflectorGrid)
    case class Energy(direction: Direction, p: Point):
      def next: List[Energy] =
        val nextPoint = p.move(direction)
        if !area.contains(nextPoint) then List.empty
        else
          reflectorGrid(nextPoint) match
            case '|' =>
              direction match
                case Up | Down | North | South => List(Energy(direction, nextPoint))
                case Left | West | Right | East =>
                  List(Energy(direction.left, nextPoint), Energy(direction.right, nextPoint))
                case _ => ???
            case '-' =>
              direction match
                case Left | West | Right | East => List(Energy(direction, nextPoint))
                case Up | Down | North | South =>
                  List(Energy(direction.left, nextPoint), Energy(direction.right, nextPoint))
                case _ => ???
            case '/' =>
              direction match
                case Up | North | Down | South  => List(Energy(direction.right, nextPoint))
                case Left | West | Right | East => List(Energy(direction.left, nextPoint))
                case _                          => ???
            case '\\' =>
              direction match
                case Up | North | Down | South  => List(Energy(direction.left, nextPoint))
                case Left | West | Right | East => List(Energy(direction.right, nextPoint))
                case _                          => ???
            case '.' => List(Energy(direction, nextPoint))
            case _   => ???
        end if
      end next

    end Energy
    def countVisited(start: Energy) =
      @tailrec
      def inner(toVisit: List[Energy], visited: Set[Energy]): Set[Energy] =
        toVisit match
          case Nil => visited
          case e :: es =>
            if visited(e) then inner(es, visited)
            else
              val next = e.next
              inner(es ++ e.next, visited + e)
      end inner
      inner(start.next, Set.empty).map(_.p).size
    end countVisited
    def bestEnergy =
      val l = area.leftBorder.points.map(p => Energy(Right, p.move(Left))).map(countVisited).max
      val r = area.rightBorder.points.map(p => Energy(Left, p.move(Right))).map(countVisited).max
      val t = area.topBorder.points.map(p => Energy(Down, p.move(Up))).map(countVisited).max
      val b = area.bottomBorder.points.map(p => Energy(Up, p.move(Down))).map(countVisited).max
      List(l, r, t, b).max
    end bestEnergy
  end Grid

  val example = Grid(""".|...\....
      ||.-.\.....
      |.....|-...
      |........|.
      |..........
      |.........\
      |..../.\\..
      |.-.-/..|..
      |.|....-|.\
      |..//.|....""".stripMargin)
  val grid = Grid(input.mkString("\n"))

  test("part1"):
    assertEquals(example.countVisited(example.Energy(Direction.Right, Point(-1, 0))), 46)
    assertEquals(grid.countVisited(grid.Energy(Direction.Right, Point(-1, 0))), 6978)

  test("part2"):
    assertEquals(example.bestEnergy, 51)
    assertEquals(grid.bestEnergy, 7315)

end Day16
