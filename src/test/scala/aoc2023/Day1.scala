package aoc2023

import scala.annotation.tailrec

class Day1 extends AocTest:

  def part1(s: String) =
    def find(take: String => Char, drop: String => String)(s: String): Int =
      if s.isEmpty then -1
      else
        val char = take(s)
        if char.isDigit then char.asDigit
        else find(take, drop)(drop(s))
    val findLastDigit  = find(_.last, _.init)
    val findFirstDigit = find(_.head, _.tail)
    findFirstDigit(s) * 10 + findLastDigit(s)
  end part1
  def part2(s: String): Int =
    val wordDigits = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine").zipWithIndex
    @tailrec
    def find(take: String => Char, drop: String => String, check: (String, String) => Boolean)(s: String): Int =
      if s.isEmpty then -1
      else
        val char = take(s)
        if char.isDigit then char.asDigit
        else
          wordDigits.find(spelledDigit => check(s, spelledDigit._1)) match
            case None    => find(take, drop, check)(drop(s))
            case Some(x) => x._2
    val findLastDigit  = find(_.last, _.init, _.endsWith(_))
    val findFirstDigit = find(_.head, _.tail, _.startsWith(_))
    findFirstDigit(s) * 10 + findLastDigit(s)
  end part2

  test("part1"):
    val example =
      """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin
    assertEquals(example.split("\n").map(part1).toSeq, Seq(12, 38, 15, 77))
    assertEquals(input.map(part1).sum, 54953)
  test("part2"):
    val example =
      """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen
      |eightsevenvqvzlqxkbm6rqhsgqpnine7twonex""".stripMargin
    assertEquals(example.split("\n").map(part2).toSeq, Seq(29, 83, 13, 24, 42, 14, 76, 81))
    assertEquals(input.map(part2).sum, 53868)

end Day1
