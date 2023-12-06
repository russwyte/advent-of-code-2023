package aoc2023
import scala.collection.immutable.SortedSet
import javax.xml.transform.Source

class Day5 extends AocTest:
  import Day5.*
  import Range.*
  case class Chopped(adjusted: Option[Range], rest: SortedSet[Range])
  def chopAndMoveRange(range: Range, intersection: Range, difference: Long): Chopped =
    val start = math.max(range.start, intersection.start)
    val end   = math.min(intersection.end, range.end)
    Range
      .getExclusive(start, end)
      .fold {
        Chopped(None, SortedSet(range))
      } { r =>
        val start           = r.start + difference
        val adjusted: Range = start until r.end + difference
        val rest = List(
          Range.getExclusive(intersection.start, range.start),
          Range.getExclusive(r.end, intersection.end),
        )
        Chopped(Some(adjusted), SortedSet(rest.flatten*))
      }
  end chopAndMoveRange
  case class RangeMapping(range: Range, start: Long):
    def chopAndMove(mask: Range): Chopped =
      chopAndMoveRange(range, mask, start - range.start)
  case class Layer(ranges: List[RangeMapping]):
    def move(range: Range): SortedSet[Range] =
      var remaining = SortedSet(range)
      var moved     = SortedSet.empty[Range]

      while remaining.nonEmpty do
        val current = remaining.firstKey
        remaining = remaining - current

        val found = this.ranges.view
          .map(r => r.chopAndMove(current))
          .collectFirst:
            case c: Chopped if c.adjusted.isDefined => c

        found match
          case None =>
            moved = moved + current
          case Some(chopped) =>
            chopped.adjusted.fold {
              moved = moved + current
            } { a =>
              remaining = remaining ++ chopped.rest
              moved = moved + a
            }
        end match
      end while

      moved
    end move
  end Layer
  def calculate(layers: List[Layer], seeds: SortedSet[Range]): Long =
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
    .toList

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
  enum Range:
    def start: Long
    def end: Long
    assert(start < end, s"Invalid range: $toString")

    // A range where the start is included and the end is excluded
    case Exlusive(start: Long, end: Long)
    // A range where the start is included and the end is also included
    case Inclusive(start: Long, end: Long)
  end Range

  object Range:
    opaque type RangedLong = Long
    extension (r: RangedLong)
      def until(end: Long): Range = Exlusive(r, end)
      def to(end: Long): Range    = Inclusive(r, end)

    def getExclusive(start: Long, end: Long): Option[Range.Exlusive] =
      Option.when(start < end)(Range.Exlusive(start, end))
    def getInclusive(start: Long, end: Long): Option[Range.Inclusive] =
      Option.when(start <= end)(Range.Inclusive(start, end))

    val r: Range = 1L until 10L
    given Ordering[Range] with
      def compare(a: Range, b: Range): Int =
        a.start.compare(b.start) match
          case 0 => a.end.compare(b.end)
          case n => n
    given Conversion[Long, RangedLong] = l => l: RangedLong
  end Range
end Day5
