package aoc

case class Interval[N: Integral](min: N, max: N):
  import math.Integral.Implicits.infixIntegralOps
  import math.Ordering.Implicits.infixOrderingOps
  import collection.immutable.NumericRange

  override def toString = s"$min..$max"
  private val one       = Integral[N].one

  def size: N                         = max + one - min
  def contains(n: N)                  = min <= n && n <= max
  def toRange: Range                  = Range.inclusive(min.toInt, max.toInt)
  def toNumericRange                  = NumericRange.inclusive[N](min, max, one)
  def iterator                        = toNumericRange.iterator
  def isSupersetOf(that: Interval[N]) = min <= that.min && that.max <= max

  def intersect(that: Interval[N]): Option[Interval[N]] =
    if isSupersetOf(that) then Some(that)
    else if that contains min then Some(copy(max = max.min(that.max)))
    else if that contains max then Some(copy(min = min.max(that.min)))
    else None

  def diff(that: Interval[N]): List[Interval[N]] =
    if min < that.min && that.max < max then List(copy(max = that.min - one), copy(min = that.max + one))
    else if that.min <= min && max <= that.max then Nil
    else if that contains max then List(copy(max = that.min - one))
    else if that contains min then List(copy(min = that.max + one))
    else List(this)

  def diff(intervals: Seq[Interval[N]]): List[Interval[N]] =
    intervals.foldLeft(List(this)) { (disjoint, n) =>
      disjoint.flatMap(_.diff(n))
    }
end Interval

object Interval:
  import math.Integral.Implicits.infixIntegralOps
  import math.Ordering.Implicits.infixOrderingOps
  import collection.immutable.NumericRange

  def apply[N: Integral](n1: N, n2: N): Interval[N] = new Interval[N](n1 min n2, n1 max n2)
  def apply(range: Range): Interval[Int]            = new Interval[Int](range.min, range.max)
  def apply[N: Integral](numericRange: NumericRange[N]): Interval[N] =
    new Interval[N](numericRange.min, numericRange.max)

  def unapply(s: String) = s match
    case s"$n1..$n2" => n1.toIntOption.zip(n2.toIntOption).map(apply(_, _))
end Interval