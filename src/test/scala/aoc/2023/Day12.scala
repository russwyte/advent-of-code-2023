package aoc.`2023`

import scala.annotation.tailrec
import aoc.*

class Day12 extends AocTest:
  import Day12.*
  val example1 =
    """???.### 1,1,3
    |.??..??...?##. 1,1,3
    |?#?#?#?#?#?#?#? 1,3,1,6
    |????.#...#... 4,1,1
    |????.######..#####. 1,6,5
    |?###???????? 3,2,1""".stripMargin
      .split("\n")
      .toVector
      .map(Record.apply)
  val records = input.map(Record.apply)

  val r  = Record("#.#.### 1,1,3")
  val r2 = Record("???.### 1,1,3")

  println(example1.map(_.possibleArrangements))
  test("part1"):
    assertEquals(example1.map(_.possibleArrangements).sum, 21L)
    assertEquals(records.map(_.possibleArrangements).sum, 7307L)
  test("part2"):
    assertEquals(records.map(_.five.possibleArrangements).sum, 3415570893842L)
end Day12

object Day12:
  import aoc.*

  case class Record(springs: String, targetDamangedGroupSizes: List[Int]):
    import Record.*
    def five: Record =
      Record(Seq.fill(5)(springs).reduce(_ ++ "?" ++ _), Seq.fill(5)(targetDamangedGroupSizes).reduce(_ ++ _))

    def possibleArrangements: Long = countArrangements(springs.toList, targetDamangedGroupSizes.toList)
  end Record
  object Record:
    private val countArrangementsMemo = scala.collection.mutable.Map.empty[(List[Char], List[Int]), Long]

    def countArrangements(springs: List[Char], sizes: List[Int]): Long =
      countArrangementsMemo.getOrElseUpdate(
        (springs, sizes), {
          (springs, sizes) match
            case (springs: List[Char], Nil) if springs.contains('#')   => 0
            case (_, Nil)                                              => 1
            case (Nil, _ :: _)                                         => 0
            case ('.' :: (newSprings: List[Char]), lengths: List[Int]) => countArrangements(newSprings, lengths)
            case ('#' :: (newSprings: List[Char]), (length: Int) :: (newLengths: List[Int]))
                if springs.length >= length =>
              val (newSpringsPrefix: List[Char], newSpringsSuffix: List[Char]) = newSprings.splitAt(length - 1)
              if newSpringsPrefix.contains('.') then 0
              else
                newSpringsSuffix match
                  case Nil                                     => countArrangements(Nil, newLengths)
                  case ('.' | '?') :: (newSprings: List[Char]) => countArrangements(newSprings, newLengths)
                  case _                                       => 0
            case ('#' :: _, _ :: _) => 0
            case ('?' :: (newSprings: List[Char]), lengths: List[Int]) =>
              countArrangements(newSprings, lengths) + countArrangements('#' :: newSprings, lengths)
            case (springs: List[Char], lengths: List[Int]) => ???
        },
      )

    def apply(s: String): Record =
      s.split(" ").toList match
        case springs :: sizes :: Nil =>
          Record(springs, sizes.split(",").map(_.toInt).toList)
        case _ => ???
  end Record

end Day12
