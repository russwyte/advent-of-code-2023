package aoc

import scala.math.Integral.Implicits.infixIntegralOps

import scala.collection.{AbstractIterator, immutable}

object Geometry:
  extension [A](it: Iterator[A])
    def zipWithTail: Iterator[(A, A)] = it
      .nextOption()
      .fold(Iterator.empty): first =>
        Iterator.unfold((first, it)): (prev, iterator) =>
          iterator
            .nextOption()
            .map: cur =>
              ((prev, cur), (cur, iterator))
  end extension

  /** Calculates the area of a simple polygon using the shoelace formula.
    * @see
    *   [[https://en.wikipedia.org/wiki/Shoelace_formula]]
    */
  def polygonArea[A](points: collection.Seq[Point])(using aIntegral: Integral[A]): A =
    ((points.last +: points).iterator.zipWithTail
      .map(_ cross _)
      .sum / aIntegral.fromInt(2)).abs

end Geometry
