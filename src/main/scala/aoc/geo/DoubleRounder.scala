package aoc.geo

// provides a way to round a Double to an Integral type
trait DoubleRounder[A](using i: Integral[A]): // just could not think of a better name
  def apply(d: Double): A

object DoubleRounder:
  given DoubleRounder[Int] with
    def apply(d: Double): Int = d.round.toInt
  given DoubleRounder[Long] with
    def apply(d: Double): Long = d.round.toLong
  given DoubleRounder[BigInt] with
    def apply(d: Double): BigInt = BigInt(d.round)
