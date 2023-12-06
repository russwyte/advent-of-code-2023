package aoc2023

import scala.math.*

class Day6 extends AocTest:
  import Day6.*

  def margine(races: List[Race]) = races.map(winningHoldRange).map(_.size).reduce(_ * _)

  def countWaysToWin(race: Race): Long = winningHoldRange(race).size

  def winningHoldRange(race: Race) =
    val a    = -1
    val b    = race.time
    val c    = -race.distance
    val D    = sqrt(b * b - 4 * a * c)
    val high = ceil((-b - D) / (2 * a)).toLong
    val low  = floor((-b + D) / (2 * a)).toLong + 1
    low until high

  import scala.math.*

  val example =
    """Time:      7  15   30
    |Distance:  9  40  200""".stripMargin
  val exampleRaces = racesSyntax.parseString(example).toOption.get
  val inputRaces   = racesSyntax.parseString(input.mkString("\n")).toOption.get
  test("part1"):
    assertEquals(margine(exampleRaces), 288)
    assertEquals(margine(inputRaces), 4811940)
  test("part2"):
    val examplePart2 =
      """Time:      71530
      |Distance:  940200""".stripMargin
    val exampleRace = racesSyntax.parseString(examplePart2).toOption.get.head
    val dStr        = inputRaces.map(_.distance.toString()).mkString
    val tStr        = inputRaces.map(_.time.toString()).mkString
    val inputRace   = Race(tStr.toLong, dStr.toLong)
    val exampleWays = countWaysToWin(exampleRace)
    val bigWays     = countWaysToWin(inputRace)
    assertEquals(exampleWays, 71503L)
    assertEquals(bigWays, 30077773L)
end Day6
object Day6:
  import zio.parser.*
  import zio.Chunk
  import AocTest.*
  case class Race(time: Long, distance: Long)
  val timesSyntax     = Syntax.string("Time: ", ()) ~ whiteSpaceSep ~ longSyntax.repeatWithSep(whiteSpaceSep).toList
  val distancesSyntax = Syntax.string("Distance: ", ()) ~ whiteSpaceSep ~ longSyntax.repeatWithSep(whiteSpaceSep).toList
  val racesSyntax =
    (timesSyntax ~ Syntax.char('\n') ~ distancesSyntax).transform(
      (t, d) => t.zip(d).map(Race.apply.tupled),
      rs =>
        val t = rs.map(_.time)
        val d = rs.map(_.distance)
        (t, d),
    )
end Day6
