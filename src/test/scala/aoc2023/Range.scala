package aoc2023

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
