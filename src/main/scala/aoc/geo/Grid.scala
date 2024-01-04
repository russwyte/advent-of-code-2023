package aoc.geo

type Grid[A] = IndexedSeq[IndexedSeq[A]]
object Grid:
  def apply[A](conv: Char => A)(str: String): Grid[A]              = str.split("\n").toIndexedSeq.map(_.map(conv))
  def apply[A](conv: Char => A)(strs: IndexedSeq[String]): Grid[A] = strs.map(s => s.map(conv))
  def lift[A, B](grid: Grid[A])(point: Point2[B])(using Integral[B]): Option[A] =
    import Integral.Implicits.infixIntegralOps
    grid.lift(point.y.toInt).flatMap(_.lift(point.x.toInt))
  extension [A](grid: Grid[A]) def get[B](point: Point2[B])(using i: Integral[B]): Option[A] = Grid.lift(grid)(point)

type CharGrid = Grid[Char]
object CharGrid:
  def apply(str: String): CharGrid              = Grid[Char](identity)(str)
  def apply(strs: IndexedSeq[String]): CharGrid = Grid[Char](identity)(strs)
  def apply(strs: String*): CharGrid            = Grid[Char](identity)(strs.toIndexedSeq)
  extension (grid: CharGrid) def get[A](point: Point2[A])(using i: Integral[A]): Option[Char] = Grid.lift(grid)(point)
