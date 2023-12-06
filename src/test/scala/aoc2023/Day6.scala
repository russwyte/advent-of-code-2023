package aoc2023

class Day6 extends AocTest:
  import Day6.*
  def winningHoldsLooping(race: Race) =
    val minWinningHold = race.distance
    val distances      = (1 to race.time.toInt).map(n => n -> n * (race.time - n))
    distances.filter((h, d) => d > race.distance).toMap
  def margine(races: List[Race]) = races.map(winningHoldsLooping).map(_.size).reduce(_ * _)

  def countWaysToWin(race: Race) = winningHoldsLooping(race).size

  import scala.annotation.tailrec

  // brutish - but I have work to do... so...
  def countWaysToWin2(race: Race): Long =
    @tailrec
    def countWays(time: Long, count: Long): Long =
      if time > race.time then count
      else
        val distance = time * (race.time - time)
        if distance > race.distance then countWays(time + 1, count + 1)
        else countWays(time + 1, count)

    countWays(1, 0)
  end countWaysToWin2

  def winningHoldRange(race: Race): Range = ???

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
    val example2 =
      """Time:      71530
      |Distance:  940200""".stripMargin
    val exampleRaces2 = racesSyntax.parseString(example2).toOption.get
    val dStr          = inputRaces.map(_.distance.toString()).mkString
    val tStr          = inputRaces.map(_.time.toString()).mkString
    val bigRacee      = Race(tStr.toLong, dStr.toLong)
    val exampleRace   = countWaysToWin2(exampleRaces2(0))
    val bigWays       = countWaysToWin2(bigRacee)
    assertEquals(exampleRace, 71503L)
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
