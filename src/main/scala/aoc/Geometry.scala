package aoc

import scala.math.Integral.Implicits.infixIntegralOps

import scala.collection.{AbstractIterator, immutable}

object Geometry:
  extension [A](it: Iterator[A])
    def zipWithTail: Iterator[(A, A)] =
      if it.hasNext then
        // TODO: can be done with unfold for Iterator?
        new AbstractIterator[(A, A)]:
          private var prev: A = it.next()

          override def hasNext: Boolean = it.hasNext

          override def next(): (A, A) =
            val cur = it.next()
            val ret = (prev, cur)
            prev = cur
            ret
      else Iterator.empty
  end extension

  /** Calculates the area of a simple polygon using the shoelace formula.
    * @see
    *   [[https://en.wikipedia.org/wiki/Shoelace_formula]]
    */
  def polygonArea[A](poss: collection.Seq[Point])(using aIntegral: Integral[A]): A =
    ((poss.last +: poss).iterator.zipWithTail
      .map(_ cross _)
      .sum / aIntegral.fromInt(2)).abs

end Geometry
