package aoc

extension (a: BigInt) def lcm(b: BigInt): BigInt = a * b / a.gcd(b)

type Grid[A] = IndexedSeq[IndexedSeq[A]]
object Grid:
  def apply[A](conv: Char => A)(str: String): Grid[A]              = apply(conv)(str.split("\n").toIndexedSeq)
  def apply[A](conv: Char => A)(strs: IndexedSeq[String]): Grid[A] = strs.map(s => s.map(conv))

type CharGrid = Grid[Char]
object CharGrid:
  def apply(str: String): CharGrid              = Grid[Char](identity)(str)
  def apply(strs: IndexedSeq[String]): CharGrid = Grid[Char](identity)(strs)
