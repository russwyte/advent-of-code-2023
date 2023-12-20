package aoc.solutions

import scala.annotation.tailrec
import aoc.*
import scala.collection.immutable.SortedSet
import scala.collection.immutable.NumericRange.Exclusive

class Day5 extends AocTest:
  import Day5.*
  def getRange(start: Long, end: Long): Option[Exclusive[Long]] =
    Option.when(start < end)(start until end)
  given Ordering[Exclusive[Long]] with
    def compare(x: Exclusive[Long], y: Exclusive[Long]): Int =
      val start = x.start.compareTo(y.start)
      if start == 0 then x.end.compareTo(y.end) else start
  case class Chopped(adjusted: Option[Exclusive[Long]], rest: SortedSet[Exclusive[Long]])
  def chopAndMoveRange(range: Exclusive[Long], intersection: Exclusive[Long], difference: Long): Chopped =
    val start = math.max(range.start, intersection.start)
    val end   = math.min(intersection.end, range.end)
    getRange(start, end)
      .fold {
        Chopped(None, SortedSet(range))
      } { r =>
        val start                     = r.start + difference
        val adjusted: Exclusive[Long] = start until r.end + difference
        val rest = List(
          getRange(intersection.start, range.start),
          getRange(r.end, intersection.end),
        )
        Chopped(Some(adjusted), SortedSet(rest.flatten*))
      }
  end chopAndMoveRange
  case class RangeMapping(range: Exclusive[Long], start: Long):
    def chopAndMove(mask: Exclusive[Long]): Chopped =
      chopAndMoveRange(range, mask, start - range.start)
  case class Layer(ranges: List[RangeMapping]):
    def move(range: Exclusive[Long]): SortedSet[Exclusive[Long]] =
      @tailrec
      def inner(
          remaining: SortedSet[Exclusive[Long]],
          moved: SortedSet[Exclusive[Long]],
      ): SortedSet[Exclusive[Long]] =
        if remaining.nonEmpty then
          val current      = remaining.firstKey
          val newRemaining = remaining - current

          val found = this.ranges.view
            .map(r => r.chopAndMove(current))
            .collectFirst:
              case c: Chopped if c.adjusted.isDefined => c

          found match
            case None =>
              inner(newRemaining, moved + current)
            case Some(chopped) =>
              chopped.adjusted match
                case None => inner(newRemaining, moved + current)
                case Some(a) =>
                  inner(newRemaining ++ chopped.rest, moved + a)
          end match
        else moved

      inner(SortedSet(range), SortedSet.empty[Exclusive[Long]])
    end move
  end Layer
  def calculate(layers: List[Layer], seeds: SortedSet[Exclusive[Long]]): Long =
    layers
      .foldLeft(seeds): (seeds, layer) =>
        seeds.flatMap(layer.move)
      .map(_.start)
      .min
  end calculate
  override val input = io.Source
    .fromResource("Day5.txt")
    .mkString
    .split("\r?\n\\s*\r?\n")
    .toVector

  val parsedInput = parsedInputSyntax.parseString(input.mkString("\n")).toOption.get
  val seeds1 = SortedSet.from(parsedInput.seeds.seeds.map: point =>
    point until point + 1)
  val seeds2 = SortedSet.from(
    parsedInput.seeds.seeds
      .sliding(2, 2)
      .collect:
        case List(a, len) => a until a + len
  )

  val layers = parsedInput.layers.map { layer =>
    val ranges = layer.rows.map: row =>
      RangeMapping(row.src until row.src + row.len, row.dest)
    Layer(ranges)
  }
  test("part1"):
    assertEquals(calculate(layers, seeds1), 175622908L)
  test("part2"):
    assertEquals(calculate(layers, seeds2), 5200543L)

end Day5
object Day5:
  import zio.parser.*
  import zio.Chunk
  import AocTest.*
  case class Seeds(seeds: List[Long])
  case class MappingRow(dest: Long, src: Long, len: Long)
  case class ParsedLayer(name: String, rows: List[MappingRow])
  case class ParsedInput(seeds: Seeds, layers: List[ParsedLayer])
  val seedsSyntax      = Syntax.string("seeds: ", ()) ~ longSyntax.repeatWithSep(whiteSpaceSep).toList
  val mappingRowSyntax = longSyntax ~ Syntax.char(' ') ~ longSyntax ~ Syntax.char(' ') ~ longSyntax
  val layerSyntax =
    (Syntax.anyChar.repeatUntil(Syntax.string(":\n", ())).toList.transform(_.mkString, _.toList) ~ mappingRowSyntax
      .transform(
        (dest, src, len) => MappingRow(dest, src, len),
        row => (row.dest, row.src, row.len),
      )
      .repeatWithSep(Syntax.char('\n'))
      .toList).transform(
      (name, rows) => ParsedLayer(name, rows),
      layer => (layer.name, layer.rows),
    )
  val parsedInputSyntax = (seedsSyntax ~ Syntax.char('\n').+.transform(_ => (), _ => Chunk.empty) ~ layerSyntax
    .repeatWithSep(Syntax.char('\n').+.toList.transform(_ => (), _ => List.empty))
    .toList).transform(
    (seeds, layers) => ParsedInput(Seeds(seeds), layers),
    input => (input.seeds.seeds, input.layers),
  )
end Day5
