package aoc.solutions

import scala.annotation.tailrec
import aoc.*

import zio.parser.*
import zio.Chunk
import scala.collection.mutable.LinkedHashSet

class Day2 extends AocTest:
  import Day2.*

  val example =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin

  val exampleBag = Bag(12, 13, 14)

  val exampleGames = example.split("\n").flatMap(gameSyntax.parseString(_).toSeq)

  val games = input.flatMap(gameSyntax.parseString(_).toSeq)

  test("part1"):
    val good =
      exampleGames.filter(_.isValid(exampleBag)).map(_.id).toSeq
    val res = games.filter(_.isValid(exampleBag)).map(_.id).sum
    assertEquals(good, Seq(1, 2, 5))
    assertEquals(res, 2727)
  test("part2"):
    val powers =
      exampleGames.map(_.power).toSeq
    val res = games.map(_.power).sum
    assertEquals(powers, Seq(48, 12, 1560, 630, 36))
    assertEquals(res, 56580)
end Day2

object Day2:
  case class Bag(red: Int, green: Int, blue: Int)

  enum PullGroup:
    def n: Int
    case Red(n: Int)
    case Green(n: Int)
    case Blue(n: Int)

  case class Pull(private val groups: LinkedHashSet[PullGroup]):
    val red   = groups.collect({ case PullGroup.Red(n) => n }).headOption.getOrElse(0)
    val green = groups.collect({ case PullGroup.Green(n) => n }).headOption.getOrElse(0)
    val blue  = groups.collect({ case PullGroup.Blue(n) => n }).headOption.getOrElse(0)

  case class Game(id: Int, pulls: Seq[Pull]):
    val minRequiredReds   = pulls.maxBy(_.red).red
    val minRequiredGreens = pulls.maxBy(_.green).green
    val minRequiredBlues  = pulls.maxBy(_.blue).blue
    val power             = minRequiredReds * minRequiredGreens * minRequiredBlues
    def isValid(bag: Bag) =
      pulls.forall(pull => pull.red <= bag.red && pull.green <= bag.green && pull.blue <= bag.blue)

  val intSyntax: Syntax[String, Char, Char, Int] = Syntax.digit.repeat.transform(
    { case chars: Chunk[Char] => chars.mkString.toInt },
    { case i: Int => Chunk.fromIterable(i.toString) },
  )

  val pullGroupSyntax: Syntax[String, Char, Char, PullGroup] =
    (intSyntax ~ (Syntax.string(" red", "red") | Syntax.string(" green", "green") | Syntax.string(" blue", "blue")))
      .transform(
        {
          case (i, "red")   => PullGroup.Red(i)
          case (i, "green") => PullGroup.Green(i)
          case (i, "blue")  => PullGroup.Blue(i)
        },
        {
          case PullGroup.Red(i)   => (i, "red")
          case PullGroup.Green(i) => (i, "green")
          case PullGroup.Blue(i)  => (i, "blue")
        },
      )

  val pullSyntax: Syntax[String, Char, Char, Pull] = pullGroupSyntax
    .repeatWithSep(Syntax.string(", ", ()))
    .transform(
      { case gs: Chunk[PullGroup] => Pull(LinkedHashSet(gs*)) },
      { case Pull(gs) => Chunk.fromIterable(gs) },
    )

  val pullChunkSyntax: Syntax[String, Char, Char, Chunk[Pull]] = pullSyntax
    .repeatWithSep(Syntax.string("; ", ()))

  val gameSyntax: Syntax[String, Char, Char, Game] =
    (Syntax.string("Game ", ()) ~ intSyntax ~ Syntax.string(": ", ()) ~ pullChunkSyntax).transform(
      { case (id, pulls) => Game(id, pulls) },
      { case Game(id, pulls) => (id, Chunk.fromIterable(pulls)) },
    )
end Day2
