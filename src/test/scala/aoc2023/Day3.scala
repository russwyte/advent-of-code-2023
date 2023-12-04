package aoc2023

import javax.swing.CellEditor
import java.awt.GridLayout

class Day3 extends AocTest:
  val example =
    """467..114..
    |...*......
    |..35..633.
    |......#...
    |617*......
    |.....+.58.
    |..592.....
    |......755.
    |...$.*....
    |.664.598..""".stripMargin

  enum Cell:
    case Empty
    case Symbol(c: Char)
    case Digit(n: Int)

  case class Point(x: Int, y: Int):
    def isAdjacentTo(other: Point): Boolean =
      (x - other.x).abs <= 1 && (y - other.y).abs <= 1

  def parseLine(line: String): Array[Cell] =
    line.map {
      case '.'            => Cell.Empty
      case c if c.isDigit => Cell.Digit(c.asDigit)
      case c              => Cell.Symbol(c)
    }.toArray

  case class IntHit(xStart: Int, xEnd: Int, y: Int, n: Int):
    val points: Seq[Point] = (xStart to xEnd).map(Point(_, y))
    def isAdjacentTo(symbolHit: SymbolHit): Boolean =
      points.exists(_.isAdjacentTo(symbolHit.point))

  case class SymbolHit(point: Point, c: Char)

  case class Gear(point: Point, n1: Int, n2: Int):
    val ratio = n1 * n2

  case class Grid(cells: Array[Array[Cell]]):
    def get(x: Int, y: Int): Cell =
      cells(y)(x)
    val width  = cells(0).length
    val height = cells.length
    val intHits: Seq[IntHit] =
      def inner(x: Int, y: Int, digits: Seq[Int], acc: Seq[IntHit]): Seq[IntHit] =
        if x >= width then
          if digits.nonEmpty then acc :+ IntHit(x - digits.length, x - 1, y, digits.mkString.toInt) else acc
        else
          val cell = get(x, y)
          cell match
            case Cell.Digit(n) =>
              inner(x + 1, y, digits :+ n, acc)
            case _ =>
              if digits.nonEmpty then
                inner(x + 1, y, Seq.empty, acc :+ IntHit(x - digits.length, x - 1, y, digits.mkString.toInt))
              else inner(x + 1, y, Seq.empty, acc)
        end if
      end inner
      (0 until height).flatMap(y => inner(0, y, Seq.empty, Seq.empty))
    end intHits
    val symbolHits: Seq[SymbolHit] =
      def inner(x: Int, y: Int, acc: Seq[SymbolHit]): Seq[SymbolHit] =
        if x >= width then acc
        else
          val cell = get(x, y)
          cell match
            case Cell.Symbol(c) =>
              inner(x + 1, y, acc :+ SymbolHit(Point(x, y), c))
            case _ =>
              inner(x + 1, y, acc)
      (0 until height).flatMap(y => inner(0, y, Seq.empty))
    end symbolHits

    val numbersAdjacentToSymbolHits: Seq[IntHit] =
      intHits.filter { intHit =>
        if intHit.n == 852 then println(intHit)
        symbolHits.exists(x => intHit.isAdjacentTo(x))
      }
    val gears =
      val possibleGears = symbolHits.filter(_.c == '*')
      possibleGears.map(pg => pg -> numbersAdjacentToSymbolHits.filter(_.isAdjacentTo(pg))).filter(_._2.size == 2).map {
        case (pg, hits) =>
          val Seq(h1, h2) = hits
          Gear(pg.point, h1.n, h2.n)
      }
  end Grid

  val exampleGrid = Grid(example)

  object Grid:
    def apply(lines: Iterable[String]): Grid =
      Grid(lines.map(parseLine).toArray)
    def apply(lines: String): Grid =
      Grid(lines.split("\n"))
  val inputGrid = Grid(input)
  test("part1"):
    assert(Point(0, 0) isAdjacentTo Point(1, 1))
    assertEquals(exampleGrid.numbersAdjacentToSymbolHits.map(_.n).sum, 4361)
    assertEquals(inputGrid.numbersAdjacentToSymbolHits.map(_.n).sum, 553825)
  test("part2"):
    assertEquals(exampleGrid.gears.map(_.ratio).sum, 467835)
    assertEquals(inputGrid.gears.size, 342)
    assertEquals(inputGrid.gears.map(_.ratio).sum, 93994191)

end Day3
